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
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, digitToInt, intToDigit, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord, toLower, toUpper)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum, alpha, digit)
import Data.Text (Text)
import qualified Data.Text as T
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
--
-- Stored in percent-decoded form.
newtype Path = Path { unPath :: ShortText }
  deriving stock (Eq, Show)

-- | URI query component (after ?).
--
-- Stored in percent-decoded form.
newtype Query = Query { unQuery :: ShortText }
  deriving stock (Eq, Show)

-- | URI fragment component (after #).
--
-- Stored in percent-decoded form.
newtype Fragment = Fragment { unFragment :: ShortText }
  deriving stock (Eq, Show)

-------------------------------------------------------------------------------
-- Character Sets (RFC 3986)
-------------------------------------------------------------------------------

-- | Unreserved characters per RFC 3986.
--
-- @
-- unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
-- @
unreservedCharSet :: CharSet
unreservedCharSet = alnum <> "-._~"

-- | Sub-delimiters per RFC 3986.
--
-- @
-- sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
-- @
subDelimsCharSet :: CharSet
subDelimsCharSet = "!$&'()*+,;="

-- | General delimiters per RFC 3986.
--
-- @
-- gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "\@"
-- @
genDelimsCharSet :: CharSet
genDelimsCharSet = ":/?#[]@"

-- | Characters allowed in scheme.
--
-- @
-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
-- @
schemeCharSet :: CharSet
schemeCharSet = alnum <> "+-."

-- | Characters allowed in userinfo (unencoded).
--
-- @
-- userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
-- @
userInfoCharSet :: CharSet
userInfoCharSet = unreservedCharSet <> subDelimsCharSet <> ":"

-- | Characters allowed in host reg-name (unencoded).
--
-- @
-- reg-name = *( unreserved / pct-encoded / sub-delims )
-- @
regNameCharSet :: CharSet
regNameCharSet = unreservedCharSet <> subDelimsCharSet

-- | Characters allowed in path segment (unencoded).
--
-- @
-- pchar = unreserved / pct-encoded / sub-delims / ":" / "\@"
-- @
pcharCharSet :: CharSet
pcharCharSet = unreservedCharSet <> subDelimsCharSet <> ":@"

-- | Characters allowed in path (unencoded).
pathCharSet :: CharSet
pathCharSet = pcharCharSet <> "/"

-- | Characters allowed in query/fragment (unencoded).
--
-- @
-- query / fragment = *( pchar / "/" / "?" )
-- @
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
--
-- A URI-reference is either a URI or a relative-reference.
parseURIReference :: ByteString -> Either String URI
parseURIReference bs = case runParser uriReferenceParser bs of
  OK uri "" -> Right uri
  OK _ rest -> Left $ "Unconsumed input after parsing URI-reference: " <> show rest
  Fail -> Left "Failed to parse URI-reference"
  Err e -> Left e

-- | Parse a complete URI.
--
-- @
-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
-- @
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
--
-- @
-- relative-ref = relative-part [ "?" query ] [ "#" fragment ]
-- @
relativeRefParser :: ParserT st String URI
relativeRefParser = do
  (auth, path) <- relativePartParser
  query <- optional ($(char '?') *> queryParser)
  fragment <- optional ($(char '#') *> fragmentParser)
  pure $ URI (Scheme "") auth path query fragment

-- | Parse scheme.
--
-- @
-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
-- @
schemeParser :: ParserT st String Scheme
schemeParser = do
  first <- satisfyAscii isAsciiAlpha
  rest <- many (satisfyAscii (`CharSet.member` schemeCharSet))
  let scheme = ST.pack (first : rest)
  pure $ Scheme scheme

-- | Parse hier-part.
--
-- @
-- hier-part = "//" authority path-abempty
--           / path-absolute
--           / path-rootless
--           / path-empty
-- @
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
--
-- @
-- authority = [ userinfo "\@" ] host [ ":" port ]
-- @
authorityParser :: ParserT st String URIAuth
authorityParser = do
  userInfo <- optional (userInfoParser <* $(char '@'))
  host <- hostParser
  port <- optional ($(char ':') *> portParser)
  pure $ URIAuth userInfo host port

-- | Parse userinfo.
userInfoParser :: ParserT st String UserInfo
userInfoParser = do
  chars <- many (pctEncodedOrChar userInfoCharSet)
  pure $ UserInfo $ ST.pack chars

-- | Parse host.
--
-- @
-- host = IP-literal / IPv4address / reg-name
-- @
hostParser :: ParserT st String Host
hostParser = ipLiteralParser <|> ipv4Parser <|> regNameParser

-- | Parse IP-literal (IPv6 or IPvFuture).
ipLiteralParser :: ParserT st String Host
ipLiteralParser = do
  $(char '[')
  content <- shortASCIIFromParser_ $ many (satisfyAscii isIPLiteralChar)
  $(char ']')
  pure $ HostIPv6 ("[" <> content <> "]")
  where
    isIPLiteralChar c = isHexDigit c || c == ':' || c == '.'

-- | Parse IPv4 address.
ipv4Parser :: ParserT st String Host
ipv4Parser = do
  -- Look ahead to check it's a valid IPv4
  o1 <- decOctet
  $(char '.')
  o2 <- decOctet
  $(char '.')
  o3 <- decOctet
  $(char '.')
  o4 <- decOctet
  -- Make sure it's not followed by more reg-name chars
  lookahead $ optional $ satisfyAscii (\c -> not (c `CharSet.member` regNameCharSet) && c /= '%')
  let addr = ST.pack $ show o1 <> "." <> show o2 <> "." <> show o3 <> "." <> show o4
  pure $ HostIPv4 addr
  where
    decOctet :: ParserT st String Int
    decOctet = do
      digits <- some (satisfyAscii isDigit)
      let n = read digits
      if n > 255
        then err "IPv4 octet out of range"
        else pure n

-- | Parse reg-name (registered name / domain).
regNameParser :: ParserT st String Host
regNameParser = do
  chars <- many (pctEncodedOrChar regNameCharSet)
  let ascii = ST.pack chars
      asciiText = ST.toText ascii
  -- Try to decode as Punycode to get Unicode form
  let unicodeForm = case IDN.toUnicode asciiText of
        Right unicode | unicode /= asciiText -> Just unicode
        _ -> Nothing
  pure $ HostRegName ascii unicodeForm

-- | Parse port.
portParser :: ParserT st String Port
portParser = do
  digits <- some (satisfyAscii isDigit)
  let n = read digits :: Int
  if n > 65535
    then err "Port number out of range"
    else pure $ Port (fromIntegral n)

-- | Parse path-abempty (may be empty).
--
-- @
-- path-abempty = *( "/" segment )
-- @
pathAbemptyParser :: ParserT st String Path
pathAbemptyParser = do
  segments <- many ($(char '/') *> segmentParser)
  let path = case segments of
        [] -> ""
        _ -> ST.pack $ "/" <> intercalate' "/" segments
  pure $ Path path

-- | Parse path-absolute.
--
-- @
-- path-absolute = "/" [ segment-nz *( "/" segment ) ]
-- @
pathAbsoluteParser :: ParserT st String Path
pathAbsoluteParser = do
  $(char '/')
  rest <- optional $ do
    first <- segmentNzParser
    more <- many ($(char '/') *> segmentParser)
    pure (first : more)
  let path = case rest of
        Nothing -> "/"
        Just segs -> ST.pack $ "/" <> intercalate' "/" segs
  pure $ Path path

-- | Parse path-rootless.
--
-- @
-- path-rootless = segment-nz *( "/" segment )
-- @
pathRootlessParser :: ParserT st String Path
pathRootlessParser = do
  first <- segmentNzParser
  more <- many ($(char '/') *> segmentParser)
  let path = ST.pack $ intercalate' "/" (first : more)
  pure $ Path path

-- | Parse path-noscheme (for relative-ref).
--
-- @
-- path-noscheme = segment-nz-nc *( "/" segment )
-- @
pathNoSchemeParser :: ParserT st String Path
pathNoSchemeParser = do
  first <- segmentNzNcParser
  more <- many ($(char '/') *> segmentParser)
  let path = ST.pack $ intercalate' "/" (first : more)
  pure $ Path path

-- | Parse empty path.
pathEmptyParser :: ParserT st String Path
pathEmptyParser = pure $ Path ""

-- | Parse segment.
segmentParser :: ParserT st String String
segmentParser = many (pctEncodedOrChar pcharCharSet)

-- | Parse segment-nz (non-zero length segment).
segmentNzParser :: ParserT st String String
segmentNzParser = some (pctEncodedOrChar pcharCharSet)

-- | Parse segment-nz-nc (non-zero, no colon).
segmentNzNcParser :: ParserT st String String
segmentNzNcParser = some (pctEncodedOrChar (pcharCharSet CharSet.\\ ":"))

-- | Parse query.
queryParser :: ParserT st String Query
queryParser = do
  chars <- many (pctEncodedOrChar queryFragmentCharSet)
  pure $ Query $ ST.pack chars

-- | Parse fragment.
fragmentParser :: ParserT st String Fragment
fragmentParser = do
  chars <- many (pctEncodedOrChar queryFragmentCharSet)
  pure $ Fragment $ ST.pack chars

-- | Parse either a percent-encoded character or a literal character from the set.
pctEncodedOrChar :: CharSet -> ParserT st String Char
pctEncodedOrChar allowed = pctEncodedChar <|> satisfyAscii (`CharSet.member` allowed)

-- | Parse a percent-encoded character.
pctEncodedChar :: ParserT st String Char
pctEncodedChar = do
  $(char '%')
  h1 <- satisfyAscii isHexDigit
  h2 <- satisfyAscii isHexDigit
  let byte = (digitToInt h1 `shiftL` 4) .|. digitToInt h2
  pure $ chr byte

-- | Helper for intercalating strings.
intercalate' :: String -> [String] -> String
intercalate' _ [] = ""
intercalate' _ [x] = x
intercalate' sep (x:xs) = x <> sep <> intercalate' sep xs

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

-- | Percent-encode a string for a given character set.
--
-- Characters in the set are left as-is; others are percent-encoded.
percentEncode :: CharSet -> Text -> Text
percentEncode allowed = T.concatMap encodeChar
  where
    encodeChar c
      | c `CharSet.member` allowed = T.singleton c
      | ord c < 128 = T.pack $ '%' : toHex (ord c)
      | otherwise = T.concat $ map (T.pack . ('%':) . toHex . fromIntegral) (B.unpack $ TE.encodeUtf8 $ T.singleton c)
    toHex n = [intToDigit (n `div` 16), intToDigit (n `mod` 16)]

-- | Percent-decode a string.
percentDecode :: Text -> Text
percentDecode = TE.decodeUtf8 . B.pack . go . T.unpack
  where
    go [] = []
    go ('%':h1:h2:rest)
      | isHexDigit h1 && isHexDigit h2 =
          let byte = fromIntegral $ (digitToInt h1 `shiftL` 4) .|. digitToInt h2
          in byte : go rest
    go (c:rest) = fromIntegral (ord c) : go rest

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
