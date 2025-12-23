{-# LANGUAGE TemplateHaskell #-}
-- | RFC 3986 compliant URI parsing with IDN support.
--
-- This module provides types and parsers for Uniform Resource Identifiers
-- as defined in RFC 3986, with support for Internationalized Domain Names
-- (IDN) per RFC 5891 (IDNA2008).
--
-- The grammar for a URI is:
--
-- @
-- URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
-- hier-part     = "//" authority path-abempty
--               / path-absolute
--               / path-rootless
--               / path-empty
-- authority     = [ userinfo "\@" ] host [ ":" port ]
-- @
--
-- For internationalized domain names (e.g., @https://münchen.de/@), we support:
--
-- * Unicode form for display: @https://münchen.de/@
-- * ASCII (Punycode) form for transmission: @https://xn--mnchen-3ya.de/@
module Network.URI
  ( -- * Types
    URI (..)
  , URIAuth (..)
  , Host (..)
  , Path (..)
  , Query (..)
  , Fragment (..)
  , Scheme (..)
  , Port (..)
  , UserInfo (..)
    -- * Parsing
  , parseURI
  , parseURIReference
  , uriParser
  , uriReferenceParser
    -- * Rendering
  , renderURI
  , renderURIUnicode
  , renderURIByteString
    -- * IDN Conversion
  , hostToASCII
  , hostToUnicode
  , uriToASCII
  , uriToUnicode
    -- * Utilities
  , percentEncode
  , percentDecode
  , isUnreserved
  , isSubDelim
  , isGenDelim
  ) where

import Control.Applicative (optional)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (chr, isAsciiLower, isAsciiUpper, isHexDigit)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import Data.Word (Word8, Word16)
import qualified Mason.Builder as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

import qualified Data.Text.IDN as IDN

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A complete URI per RFC 3986.
--
-- @
-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
-- @
data URI = URI
  { uriScheme :: !Scheme
  -- ^ The URI scheme (e.g., "https", "http", "ftp")
  , uriAuthority :: !(Maybe URIAuth)
  -- ^ Optional authority component
  , uriPath :: !Path
  -- ^ The path component
  , uriQuery :: !(Maybe Query)
  -- ^ Optional query component
  , uriFragment :: !(Maybe Fragment)
  -- ^ Optional fragment component
  }
  deriving stock (Eq, Show)

-- | URI authority component.
--
-- @
-- authority = [ userinfo "\@" ] host [ ":" port ]
-- @
data URIAuth = URIAuth
  { uriUserInfo :: !(Maybe UserInfo)
  -- ^ Optional user information (e.g., "user:password")
  , uriHost :: !Host
  -- ^ The host (domain name, IPv4, or IPv6)
  , uriPort :: !(Maybe Port)
  -- ^ Optional port number
  }
  deriving stock (Eq, Show)

-- | URI host component.
--
-- For internationalized domains, stores both ASCII and Unicode forms.
data Host
  = HostRegName
      { hostASCII :: !ShortText
      -- ^ ASCII (A-label/Punycode) form
      , hostUnicode :: !(Maybe Text)
      -- ^ Unicode (U-label) form, if different
      }
  | HostIPv4 !ShortText
  -- ^ IPv4 address (e.g., "192.168.1.1")
  | HostIPv6 !ShortText
  -- ^ IPv6 address (e.g., "[::1]")
  deriving stock (Eq, Show)

-- | URI scheme (e.g., "http", "https", "ftp").
newtype Scheme = Scheme { unScheme :: ShortText }
  deriving stock (Eq, Show)

-- | URI port number.
newtype Port = Port { unPort :: Word16 }
  deriving stock (Eq, Show)

-- | URI user info (everything before @ in authority).
newtype UserInfo = UserInfo { unUserInfo :: ShortText }
  deriving stock (Eq, Show)

-- | URI path component.
newtype Path = Path { unPath :: ShortText }
  deriving stock (Eq, Show)

-- | URI query component (after ?).
newtype Query = Query { unQuery :: ShortText }
  deriving stock (Eq, Show)

-- | URI fragment component (after #).
newtype Fragment = Fragment { unFragment :: ShortText }
  deriving stock (Eq, Show)

-------------------------------------------------------------------------------
-- Character Sets (RFC 3986)
-------------------------------------------------------------------------------

-- | Unreserved characters per RFC 3986.
unreservedCharSet :: CharSet
unreservedCharSet = alnum <> "-._~"

-- | Sub-delimiters per RFC 3986.
subDelimsCharSet :: CharSet
subDelimsCharSet = "!$&'()*+,;="

-- | General delimiters per RFC 3986.
genDelimsCharSet :: CharSet
genDelimsCharSet = ":/?#[]@"

-- | Characters allowed in scheme.
schemeCharSet :: CharSet
schemeCharSet = alnum <> "+-."

-- | Characters allowed in userinfo (unencoded).
userInfoCharSet :: CharSet
userInfoCharSet = unreservedCharSet <> subDelimsCharSet <> ":"

-- | Characters allowed in host reg-name (unencoded).
regNameCharSet :: CharSet
regNameCharSet = unreservedCharSet <> subDelimsCharSet

-- | Characters allowed in path segment (unencoded).
pcharCharSet :: CharSet
pcharCharSet = unreservedCharSet <> subDelimsCharSet <> ":@"

-- | Characters allowed in query/fragment (unencoded).
queryFragmentCharSet :: CharSet
queryFragmentCharSet = pcharCharSet <> "/?"

-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------

-- | Check if a character is unreserved per RFC 3986.
isUnreserved :: Char -> Bool
isUnreserved c = c `CharSet.member` unreservedCharSet

-- | Check if a character is a sub-delimiter per RFC 3986.
isSubDelim :: Char -> Bool
isSubDelim c = c `CharSet.member` subDelimsCharSet

-- | Check if a character is a general delimiter per RFC 3986.
isGenDelim :: Char -> Bool
isGenDelim c = c `CharSet.member` genDelimsCharSet

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Parse a URI from a ByteString.
parseURI :: ByteString -> Either String URI
parseURI bs = case runParser uriParser bs of
  OK uri "" -> Right uri
  OK _ rest -> Left $ "Unconsumed input after parsing URI: " <> show rest
  Fail -> Left "Failed to parse URI"
  Err e -> Left e

-- | Parse a URI-reference from a ByteString.
parseURIReference :: ByteString -> Either String URI
parseURIReference bs = case runParser uriReferenceParser bs of
  OK uri "" -> Right uri
  OK _ rest -> Left $ "Unconsumed input after parsing URI-reference: " <> show rest
  Fail -> Left "Failed to parse URI-reference"
  Err e -> Left e

-- | Parse a complete URI.
uriParser :: ParserT st String URI
uriParser = do
  scheme <- schemeParser
  $(char ':')
  (auth, path) <- hierPartParser
  query <- optional ($(char '?') *> queryParser)
  fragment <- optional ($(char '#') *> fragmentParser)
  pure $ URI scheme auth path query fragment

-- | Parse a URI-reference (URI or relative-reference).
uriReferenceParser :: ParserT st String URI
uriReferenceParser = uriParser <|> relativeRefParser

-- | Parse a relative-reference.
relativeRefParser :: ParserT st String URI
relativeRefParser = do
  (auth, path) <- relativePartParser
  query <- optional ($(char '?') *> queryParser)
  fragment <- optional ($(char '#') *> fragmentParser)
  pure $ URI (Scheme "") auth path query fragment

-- | Parse scheme - uses shortASCIIFromParser_ to avoid String allocation.
schemeParser :: ParserT st String Scheme
schemeParser = Scheme <$> shortASCIIFromParser_ schemeChars
  where
    schemeChars = do
      skipSatisfyAscii isAsciiAlpha
      skipMany (skipSatisfyAscii (`CharSet.member` schemeCharSet))

-- | Parse hier-part.
hierPartParser :: ParserT st String (Maybe URIAuth, Path)
hierPartParser = withAuth <|> pathOnly
  where
    withAuth = do
      $(string "//")
      auth <- authorityParser
      path <- pathAbemptyParser
      pure (Just auth, path)
    pathOnly = do
      path <- pathAbsoluteParser <|> pathRootlessParser <|> pathEmptyParser
      pure (Nothing, path)

-- | Parse relative-part.
relativePartParser :: ParserT st String (Maybe URIAuth, Path)
relativePartParser = withAuth <|> pathOnly
  where
    withAuth = do
      $(string "//")
      auth <- authorityParser
      path <- pathAbemptyParser
      pure (Just auth, path)
    pathOnly = do
      path <- pathAbsoluteParser <|> pathNoSchemeParser <|> pathEmptyParser
      pure (Nothing, path)

-- | Parse authority.
authorityParser :: ParserT st String URIAuth
authorityParser = do
  userInfo <- optional (userInfoParser <* $(char '@'))
  host <- hostParser
  port <- optional ($(char ':') *> portParser)
  pure $ URIAuth userInfo host port

-- | Parse userinfo - captures bytes directly.
userInfoParser :: ParserT st String UserInfo
userInfoParser = UserInfo <$> shortASCIIFromParser_ (skipMany userInfoChar)
  where
    userInfoChar = skipSatisfyAscii (`CharSet.member` userInfoCharSet) <|> pctEncodedSkip

-- | Parse host.
hostParser :: ParserT st String Host
hostParser = ipLiteralParser <|> ipv4Parser <|> regNameParser

-- | Parse IP-literal (IPv6 or IPvFuture).
ipLiteralParser :: ParserT st String Host
ipLiteralParser = do
  content <- shortASCIIFromParser_ $ do
    $(char '[')
    skipMany (skipSatisfyAscii isIPLiteralChar)
    $(char ']')
  pure $ HostIPv6 content
  where
    isIPLiteralChar c = isHexDigit c || c == ':' || c == '.'

-- | Parse IPv4 address - uses anyAsciiDecimalWord for efficiency.
ipv4Parser :: ParserT st String Host
ipv4Parser = do
  addr <- shortASCIIFromParser_ $ do
    decOctet >> $(char '.') >> decOctet >> $(char '.') >> decOctet >> $(char '.') >> decOctet
  -- Make sure it's not followed by more reg-name chars
  lookahead $ optional $ satisfyAscii (\c -> not (c `CharSet.member` regNameCharSet) && c /= '%')
  pure $ HostIPv4 addr
  where
    decOctet :: ParserT st String ()
    decOctet = do
      n <- anyAsciiDecimalWord
      if n > 255
        then err "IPv4 octet out of range"
        else pure ()

-- | Parse reg-name (registered name / domain).
regNameParser :: ParserT st String Host
regNameParser = do
  ascii <- shortASCIIFromParser_ (skipMany regNameChar)
  let asciiText = ST.toText ascii
  -- Try to decode as Punycode to get Unicode form
  let unicodeForm = case IDN.toUnicode asciiText of
        Right unicode | unicode /= asciiText -> Just unicode
        _ -> Nothing
  pure $ HostRegName ascii unicodeForm
  where
    regNameChar = skipSatisfyAscii (`CharSet.member` regNameCharSet) <|> pctEncodedSkip

-- | Parse port - uses anyAsciiDecimalWord for efficiency.
portParser :: ParserT st String Port
portParser = do
  n <- anyAsciiDecimalWord
  if n > 65535
    then err "Port number out of range"
    else pure $ Port (fromIntegral n)

-- | Parse path-abempty (may be empty).
pathAbemptyParser :: ParserT st String Path
pathAbemptyParser = Path <$> shortASCIIFromParser_ (skipMany segment)
  where
    segment = $(char '/') >> skipMany pcharSkip

-- | Parse path-absolute.
pathAbsoluteParser :: ParserT st String Path
pathAbsoluteParser = Path <$> shortASCIIFromParser_ pathContent
  where
    pathContent = do
      $(char '/')
      optional_ $ do
        skipSome pcharSkip  -- segment-nz
        skipMany ($(char '/') >> skipMany pcharSkip)

-- | Parse path-rootless.
pathRootlessParser :: ParserT st String Path
pathRootlessParser = Path <$> shortASCIIFromParser_ pathContent
  where
    pathContent = do
      skipSome pcharSkip  -- segment-nz
      skipMany ($(char '/') >> skipMany pcharSkip)

-- | Parse path-noscheme (for relative-ref).
pathNoSchemeParser :: ParserT st String Path
pathNoSchemeParser = Path <$> shortASCIIFromParser_ pathContent
  where
    pathContent = do
      skipSome segmentNzNcChar  -- segment-nz-nc (no colon)
      skipMany ($(char '/') >> skipMany pcharSkip)
    segmentNzNcChar = skipSatisfyAscii (`CharSet.member` (pcharCharSet CharSet.\\ ":")) <|> pctEncodedSkip

-- | Parse empty path.
pathEmptyParser :: ParserT st String Path
pathEmptyParser = pure $ Path ""

-- | Parse query.
queryParser :: ParserT st String Query
queryParser = Query <$> shortASCIIFromParser_ (skipMany queryChar)
  where
    queryChar = skipSatisfyAscii (`CharSet.member` queryFragmentCharSet) <|> pctEncodedSkip

-- | Parse fragment.
fragmentParser :: ParserT st String Fragment
fragmentParser = Fragment <$> shortASCIIFromParser_ (skipMany fragmentChar)
  where
    fragmentChar = skipSatisfyAscii (`CharSet.member` queryFragmentCharSet) <|> pctEncodedSkip

-- | Skip a pchar (path character).
pcharSkip :: ParserT st String ()
pcharSkip = skipSatisfyAscii (`CharSet.member` pcharCharSet) <|> pctEncodedSkip

-- | Skip a percent-encoded sequence.
pctEncodedSkip :: ParserT st String ()
pctEncodedSkip = do
  $(char '%')
  skipSatisfyAscii isHexDigit
  skipSatisfyAscii isHexDigit

-- | Optional that returns unit.
optional_ :: ParserT st e a -> ParserT st e ()
optional_ p = (p >> pure ()) <|> pure ()

-- | Check if character is ASCII alpha.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

-------------------------------------------------------------------------------
-- IDN Conversion
-------------------------------------------------------------------------------

-- | Convert host to ASCII (Punycode) form.
hostToASCII :: Host -> ShortText
hostToASCII (HostRegName ascii _) = ascii
hostToASCII (HostIPv4 addr) = addr
hostToASCII (HostIPv6 addr) = addr

-- | Convert host to Unicode form for display.
hostToUnicode :: Host -> Text
hostToUnicode (HostRegName ascii mUnicode) =
  case mUnicode of
    Just unicode -> unicode
    Nothing -> ST.toText ascii
hostToUnicode (HostIPv4 addr) = ST.toText addr
hostToUnicode (HostIPv6 addr) = ST.toText addr

-- | Convert URI to use ASCII host (for transmission).
uriToASCII :: URI -> URI
uriToASCII uri = uri { uriAuthority = fmap authToASCII (uriAuthority uri) }
  where
    authToASCII auth = auth { uriHost = hostToASCIIHost (uriHost auth) }
    hostToASCIIHost (HostRegName ascii _) = HostRegName ascii Nothing
    hostToASCIIHost h = h

-- | URI with Unicode host preserved (already stored).
uriToUnicode :: URI -> URI
uriToUnicode = id

-- | Create a host from Unicode text.
mkHostFromUnicode :: Text -> Either String Host
mkHostFromUnicode txt = case IDN.toASCII txt of
  Left e -> Left $ "Invalid internationalized domain name: " <> show e
  Right ascii ->
    let asciiShort = ST.fromText ascii
        unicodeForm = if ascii == txt then Nothing else Just txt
    in Right $ HostRegName asciiShort unicodeForm

-------------------------------------------------------------------------------
-- Percent Encoding/Decoding
-------------------------------------------------------------------------------

-- | Percent-encode text for a given character set.
percentEncode :: CharSet -> Text -> Text
percentEncode allowed = TE.decodeUtf8 . B.concatMap encodeByte . TE.encodeUtf8
  where
    encodeByte :: Word8 -> ByteString
    encodeByte b
      | b < 128 && chr (fromIntegral b) `CharSet.member` allowed = B.singleton b
      | otherwise = B.pack [0x25, toHexUpper (b `div` 16), toHexUpper (b `mod` 16)]  -- 0x25 = '%'
    toHexUpper n
      | n < 10 = 0x30 + n  -- '0'
      | otherwise = 0x41 + n - 10  -- 'A'

-- | Percent-decode text.
percentDecode :: Text -> Text
percentDecode = TE.decodeUtf8 . percentDecodeBS . TE.encodeUtf8

-- | Percent-decode a ByteString.
percentDecodeBS :: ByteString -> ByteString
percentDecodeBS bs = B.pack $ go (B.unpack bs)
  where
    go [] = []
    go (0x25:h1:h2:rest)  -- '%'
      | isHexDigitW8 h1 && isHexDigitW8 h2 =
          let byte = (fromHex h1 `shiftL` 4) .|. fromHex h2
          in byte : go rest
    go (b:rest) = b : go rest

    isHexDigitW8 b = (b >= 0x30 && b <= 0x39) ||  -- 0-9
                     (b >= 0x41 && b <= 0x46) ||  -- A-F
                     (b >= 0x61 && b <= 0x66)     -- a-f

    fromHex b
      | b >= 0x30 && b <= 0x39 = b - 0x30        -- 0-9
      | b >= 0x41 && b <= 0x46 = b - 0x41 + 10   -- A-F
      | otherwise = b - 0x61 + 10                -- a-f

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

-- | Render a URI to a Builder (uses ASCII/Punycode host).
renderURI :: URI -> M.Builder
renderURI (URI scheme mAuth path mQuery mFragment) =
  renderScheme scheme <>
  maybe mempty renderAuthority mAuth <>
  renderPath path <>
  maybe mempty renderQuery mQuery <>
  maybe mempty renderFragment mFragment

-- | Render a URI with Unicode host (for display).
renderURIUnicode :: URI -> M.Builder
renderURIUnicode (URI scheme mAuth path mQuery mFragment) =
  renderScheme scheme <>
  maybe mempty renderAuthorityUnicode mAuth <>
  renderPath path <>
  maybe mempty renderQuery mQuery <>
  maybe mempty renderFragment mFragment

-- | Render a URI to a strict ByteString.
renderURIByteString :: URI -> ByteString
renderURIByteString = M.toStrictByteString . renderURI

renderScheme :: Scheme -> M.Builder
renderScheme (Scheme s)
  | ST.null s = mempty
  | otherwise = shortText s <> ":"

renderAuthority :: URIAuth -> M.Builder
renderAuthority (URIAuth mUserInfo host mPort) =
  "//" <>
  maybe mempty renderUserInfo mUserInfo <>
  renderHost host <>
  maybe mempty renderPort mPort

renderAuthorityUnicode :: URIAuth -> M.Builder
renderAuthorityUnicode (URIAuth mUserInfo host mPort) =
  "//" <>
  maybe mempty renderUserInfo mUserInfo <>
  renderHostUnicode host <>
  maybe mempty renderPort mPort

renderUserInfo :: UserInfo -> M.Builder
renderUserInfo (UserInfo ui) = shortText ui <> "@"

renderHost :: Host -> M.Builder
renderHost (HostRegName ascii _) = shortText ascii
renderHost (HostIPv4 addr) = shortText addr
renderHost (HostIPv6 addr) = shortText addr

renderHostUnicode :: Host -> M.Builder
renderHostUnicode (HostRegName ascii mUnicode) =
  case mUnicode of
    Just unicode -> M.textUtf8 unicode
    Nothing -> shortText ascii
renderHostUnicode (HostIPv4 addr) = shortText addr
renderHostUnicode (HostIPv6 addr) = shortText addr

renderPort :: Port -> M.Builder
renderPort (Port p) = ":" <> M.word16Dec p

renderPath :: Path -> M.Builder
renderPath (Path p) = shortText p

renderQuery :: Query -> M.Builder
renderQuery (Query q) = "?" <> shortText q

renderFragment :: Fragment -> M.Builder
renderFragment (Fragment f) = "#" <> shortText f
