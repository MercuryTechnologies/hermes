{-# LANGUAGE TemplateHaskell #-}
-- | Email address (mailbox) parsing per RFC 5322 with IDN support.
--
-- This module provides types and parsers for Internet email addresses
-- as defined in RFC 5322 (Internet Message Format), with support for
-- Internationalized Domain Names (IDN) per RFC 5891 (IDNA2008).
--
-- The grammar for a mailbox is:
--
-- @
-- mailbox         =   name-addr / addr-spec
-- name-addr       =   [display-name] angle-addr
-- angle-addr      =   [CFWS] "\<" addr-spec "\>" [CFWS]
-- display-name    =   phrase
-- addr-spec       =   local-part "\@" domain
-- local-part      =   dot-atom / quoted-string
-- domain          =   dot-atom / domain-literal
-- @
--
-- For internationalized domains (e.g., @münchen.de@), we support both:
--
-- * Unicode form for display: @user\@münchen.de@
-- * ASCII (Punycode) form for transmission: @user\@xn--mnchen-3ya.de@
module Network.Mailbox
  ( -- * Types
    Mailbox (..)
  , AddrSpec (..)
  , LocalPart (..)
  , Domain (..)
    -- * Parsing
  , mailboxParser
  , addrSpecParser
  , parseMailbox
  , parseAddrSpec
    -- * Rendering
  , renderMailbox
  , renderAddrSpec
  , renderMailboxUnicode
  , renderAddrSpecUnicode
    -- * IDN Conversion
  , domainToASCII
  , domainToUnicode
  , mkDomainFromUnicode
  , addrSpecToASCII
  , addrSpecToUnicode
  , mailboxToASCII
  , mailboxToUnicode
  ) where

import Control.Applicative (optional)
import Data.ByteString (ByteString)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

import qualified Data.Text.IDN as IDN

-- | A complete mailbox, which may include a display name.
--
-- Examples:
--
-- * @john\@example.com@ (addr-spec only)
-- * @John Doe \<john\@example.com\>@ (with display name)
-- * @user\@münchen.de@ (with internationalized domain)
data Mailbox = Mailbox
  { mailboxDisplayName :: !(Maybe Text)
  -- ^ Optional display name (e.g., "John Doe")
  , mailboxAddrSpec :: !AddrSpec
  -- ^ The actual email address
  }
  deriving stock (Eq, Show)

-- | An email address specification (local-part\@domain).
--
-- Example: @john\@example.com@
data AddrSpec = AddrSpec
  { addrSpecLocalPart :: !LocalPart
  -- ^ The local part before the \@
  , addrSpecDomain :: !Domain
  -- ^ The domain after the \@
  }
  deriving stock (Eq, Show)

-- | The local part of an email address (before the \@).
--
-- Can be either a dot-atom (e.g., @john.doe@) or a quoted string
-- (e.g., @"john doe"@).
data LocalPart
  = LocalPartDotAtom !ShortText
  -- ^ A dot-atom local part (most common)
  | LocalPartQuoted !ShortText
  -- ^ A quoted-string local part (for special characters)
  deriving stock (Eq, Show)

-- | The domain part of an email address (after the \@).
--
-- Can be either a domain name (e.g., @example.com@) or a domain literal
-- (e.g., @[192.168.1.1]@).
--
-- For internationalized domains, we store both the ASCII (Punycode) form
-- and the Unicode form when they differ:
--
-- * @DomainName "xn--mnchen-3ya.de" (Just "münchen.de")@ - IDN domain
-- * @DomainName "example.com" Nothing@ - ASCII-only domain
data Domain
  = DomainName
      { domainASCII :: !ShortText
      -- ^ The ASCII (A-label/Punycode) form, safe for transmission
      , domainUnicode :: !(Maybe Text)
      -- ^ The Unicode (U-label) form, if different from ASCII
      }
  | DomainLiteral !ShortText
  -- ^ A domain literal (e.g., @[192.168.1.1]@)
  deriving stock (Eq, Show)

-- | Parse a mailbox from a ByteString.
parseMailbox :: ByteString -> Either String Mailbox
parseMailbox bs = case runParser mailboxParser bs of
  OK mailbox "" -> Right mailbox
  OK _ rest -> Left $ "Unconsumed input after parsing mailbox: " <> show rest
  Fail -> Left "Failed to parse mailbox"
  Err e -> Left e

-- | Parse an addr-spec from a ByteString.
parseAddrSpec :: ByteString -> Either String AddrSpec
parseAddrSpec bs = case runParser addrSpecParser bs of
  OK addr "" -> Right addr
  OK _ rest -> Left $ "Unconsumed input after parsing addr-spec: " <> show rest
  Fail -> Left "Failed to parse addr-spec"
  Err e -> Left e

-- | Character set for atext (atom text) per RFC 5322.
--
-- @
-- atext = ALPHA / DIGIT /
--         "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "/" /
--         "=" / "?" / "^" / "_" / "`" / "{" / "|" / "}" / "~"
-- @
atextCharSet :: CharSet
atextCharSet = alnum <> "!#$%&'*+-/=?^_`{|}~"

-- | Character set for characters allowed in domain names.
--
-- Domain names use a subset: alphanumeric and hyphen.
domainCharSet :: CharSet
domainCharSet = alnum <> "-"

-- | Character set for quoted-pair characters.
--
-- Characters that can appear after a backslash in a quoted string.
quotedPairCharSet' :: CharSet
quotedPairCharSet' = CharSet.fromList ['\x21'..'\x7E'] <> "\t "

-- | Character set for qtext (quoted text without escaping).
--
-- @
-- qtext = %d33 / %d35-91 / %d93-126
-- @
qtextCharSet :: CharSet
qtextCharSet = CharSet.singleton '\x21' <>
               CharSet.fromList ['\x23'..'\x5B'] <>
               CharSet.fromList ['\x5D'..'\x7E']

-- | Character set for dtext (domain literal text).
--
-- @
-- dtext = %d33-90 / %d94-126
-- @
dtextCharSet :: CharSet
dtextCharSet = CharSet.fromList ['\x21'..'\x5A'] <>
               CharSet.fromList ['\x5E'..'\x7E']

-- | Parse a mailbox.
--
-- @
-- mailbox = name-addr / addr-spec
-- name-addr = [display-name] angle-addr
-- @
mailboxParser :: ParserT st String Mailbox
mailboxParser = do
  cfws
  nameAddrParser <|> bareAddrSpecParser
  where
    -- Parse name-addr form: [display-name] <addr-spec>
    nameAddrParser = do
      displayName <- optional displayNameParser
      cfws
      $(char '<')
      cfws
      addr <- addrSpecParser
      cfws
      $(char '>')
      cfws
      pure $ Mailbox displayName addr

    -- Parse bare addr-spec (no angle brackets)
    bareAddrSpecParser = do
      addr <- addrSpecParser
      cfws
      pure $ Mailbox Nothing addr

-- | Parse display name (phrase).
--
-- A phrase is one or more words separated by whitespace.
-- Words can be atoms or quoted strings.
displayNameParser :: ParserT st String Text
displayNameParser = do
  words' <- some wordParser
  pure $ T.intercalate " " words'
  where
    wordParser = do
      cfws
      w <- atomTextParser <|> quotedStringText
      cfws
      pure w

    atomTextParser = do
      chars <- some (satisfyAscii (`CharSet.member` atextCharSet))
      pure $ T.pack chars

    quotedStringText = do
      $(char '"')
      content <- many (unescapedQChar <|> escapedChar)
      $(char '"')
      pure $ T.pack content

    unescapedQChar = satisfyAscii (`CharSet.member` qtextCharSet)
    escapedChar = $(char '\\') *> satisfyAscii (`CharSet.member` quotedPairCharSet')

-- | Parse an addr-spec.
--
-- @
-- addr-spec = local-part "\@" domain
-- @
addrSpecParser :: ParserT st String AddrSpec
addrSpecParser = do
  local <- localPartParser
  $(char '@')
  domain <- domainParser
  pure $ AddrSpec local domain

-- | Parse the local part of an email address.
--
-- @
-- local-part = dot-atom / quoted-string
-- @
localPartParser :: ParserT st String LocalPart
localPartParser = quotedLocalPart <|> dotAtomLocalPart
  where
    dotAtomLocalPart = LocalPartDotAtom <$> dotAtomParser
    quotedLocalPart = LocalPartQuoted <$> quotedStringParser

-- | Parse a dot-atom.
--
-- @
-- dot-atom-text = 1*atext *("." 1*atext)
-- @
dotAtomParser :: ParserT st String ShortText
dotAtomParser = shortASCIIFromParser_ $ do
  atomPart
  many ($(char '.') *> atomPart)
  where
    atomPart = some (satisfyAscii (`CharSet.member` atextCharSet))

-- | Parse a quoted string for local part.
quotedStringParser :: ParserT st String ShortText
quotedStringParser = do
  $(char '"')
  content <- many (unescapedQChar <|> escapedChar)
  $(char '"')
  pure $ ST.pack content
  where
    unescapedQChar = satisfyAscii (`CharSet.member` qtextCharSet)
    escapedChar = $(char '\\') *> satisfyAscii (`CharSet.member` quotedPairCharSet')

-- | Parse the domain part of an email address.
--
-- @
-- domain = dot-atom / domain-literal
-- @
--
-- This parser accepts both ASCII domains and Punycode-encoded IDN domains.
-- For pure ASCII domains, Unicode form is not stored.
-- For Punycode domains (starting with xn--), we decode to Unicode.
domainParser :: ParserT st String Domain
domainParser = domainLiteralParser <|> domainNameParser
  where
    domainNameParser = do
      ascii <- domainDotAtomParser
      let asciiText = ST.toText ascii
      -- Try to decode as Punycode to get Unicode form
      let unicodeForm = case IDN.toUnicode asciiText of
            Right unicode | unicode /= asciiText -> Just unicode
            _ -> Nothing
      pure $ DomainName ascii unicodeForm
    domainLiteralParser = DomainLiteral <$> domainLiteralParser'

-- | Parse a domain name (dot-atom for domains).
--
-- Domain names are more restricted than local-part dot-atoms.
domainDotAtomParser :: ParserT st String ShortText
domainDotAtomParser = shortASCIIFromParser_ $ do
  domainLabel
  many ($(char '.') *> domainLabel)
  where
    -- Domain labels must start and end with alphanumeric
    domainLabel = do
      first <- satisfyAscii (`CharSet.member` alnum)
      rest <- many (satisfyAscii (`CharSet.member` domainCharSet))
      -- Ensure last char is alphanumeric if there are more chars
      case rest of
        [] -> pure ()
        _ -> if last rest `CharSet.member` alnum
             then pure ()
             else err "Domain label must end with alphanumeric character"

-- | Parse a domain literal.
--
-- @
-- domain-literal = "[" *dtext "]"
-- @
domainLiteralParser' :: ParserT st String ShortText
domainLiteralParser' = do
  $(char '[')
  content <- shortASCIIFromParser_ $ many (satisfyAscii (`CharSet.member` dtextCharSet))
  $(char ']')
  pure $ "[" <> content <> "]"

-- | Comment and folding whitespace (simplified).
--
-- For HTTP headers, we just skip optional whitespace.
cfws :: ParserT st e ()
cfws = ows

-------------------------------------------------------------------------------
-- IDN Conversion Functions
-------------------------------------------------------------------------------

-- | Convert a domain to its ASCII (Punycode) form.
--
-- Returns the ASCII form suitable for transmission over the wire.
-- For domain literals, returns as-is.
domainToASCII :: Domain -> ShortText
domainToASCII (DomainName ascii _) = ascii
domainToASCII (DomainLiteral lit) = lit

-- | Convert a domain to its Unicode form for display.
--
-- Returns the Unicode form if available, otherwise the ASCII form.
-- For domain literals, returns as-is.
domainToUnicode :: Domain -> Text
domainToUnicode (DomainName ascii mUnicode) =
  case mUnicode of
    Just unicode -> unicode
    Nothing -> ST.toText ascii
domainToUnicode (DomainLiteral lit) = ST.toText lit

-- | Convert an addr-spec to use ASCII domain (for transmission).
addrSpecToASCII :: AddrSpec -> AddrSpec
addrSpecToASCII addr@(AddrSpec _ (DomainLiteral _)) = addr
addrSpecToASCII (AddrSpec local (DomainName ascii _)) =
  AddrSpec local (DomainName ascii Nothing)

-- | Try to convert a Unicode domain to its ASCII form and create an AddrSpec.
--
-- This is useful when you have a Unicode email address and need to convert
-- it for transmission.
addrSpecToUnicode :: AddrSpec -> AddrSpec
addrSpecToUnicode = id  -- Already stores Unicode form if available

-- | Convert a mailbox to use ASCII domain (for transmission).
mailboxToASCII :: Mailbox -> Mailbox
mailboxToASCII (Mailbox displayName addr) =
  Mailbox displayName (addrSpecToASCII addr)

-- | Convert a mailbox, keeping Unicode domain form.
mailboxToUnicode :: Mailbox -> Mailbox
mailboxToUnicode = id  -- Already stores Unicode form if available

-- | Create a domain from a Unicode text, converting to ASCII (Punycode) form.
--
-- This handles internationalized domain names like @münchen.de@.
mkDomainFromUnicode :: Text -> Either String Domain
mkDomainFromUnicode txt = case IDN.toASCII txt of
  Left e -> Left $ "Invalid internationalized domain name: " <> show e
  Right ascii ->
    let asciiShort = ST.fromText ascii
        -- Store Unicode form only if different from ASCII
        unicodeForm = if ascii == txt then Nothing else Just txt
    in Right $ DomainName asciiShort unicodeForm

-------------------------------------------------------------------------------
-- Rendering Functions
-------------------------------------------------------------------------------

-- | Render a mailbox to a Builder (uses ASCII/Punycode domain).
--
-- This produces output safe for transmission over the wire.
renderMailbox :: Mailbox -> M.Builder
renderMailbox (Mailbox mDisplayName addrSpec) = case mDisplayName of
  Nothing -> renderAddrSpec addrSpec
  Just displayName ->
    renderDisplayName displayName <> " <" <> renderAddrSpec addrSpec <> ">"
  where
    renderDisplayName name
      | T.any needsQuoting name = "\"" <> escapeQuoted name <> "\""
      | otherwise = M.textUtf8 name

    needsQuoting c = c == '"' || c == '\\' || c == '<' || c == '>' ||
                     c == '@' || c == ',' || c == ';' || c == ':'

    escapeQuoted = M.textUtf8 . T.concatMap escapeChar
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

-- | Render a mailbox with Unicode domain (for display to users).
renderMailboxUnicode :: Mailbox -> M.Builder
renderMailboxUnicode (Mailbox mDisplayName addrSpec) = case mDisplayName of
  Nothing -> renderAddrSpecUnicode addrSpec
  Just displayName ->
    renderDisplayName displayName <> " <" <> renderAddrSpecUnicode addrSpec <> ">"
  where
    renderDisplayName name
      | T.any needsQuoting name = "\"" <> escapeQuoted name <> "\""
      | otherwise = M.textUtf8 name

    needsQuoting c = c == '"' || c == '\\' || c == '<' || c == '>' ||
                     c == '@' || c == ',' || c == ';' || c == ':'

    escapeQuoted = M.textUtf8 . T.concatMap escapeChar
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

-- | Render an addr-spec to a Builder (uses ASCII/Punycode domain).
--
-- This produces output safe for transmission over the wire.
renderAddrSpec :: AddrSpec -> M.Builder
renderAddrSpec (AddrSpec localPart domain) =
  renderLocalPart localPart <> "@" <> renderDomain domain

-- | Render an addr-spec with Unicode domain (for display to users).
renderAddrSpecUnicode :: AddrSpec -> M.Builder
renderAddrSpecUnicode (AddrSpec localPart domain) =
  renderLocalPart localPart <> "@" <> renderDomainUnicode domain

-- | Render a local part.
renderLocalPart :: LocalPart -> M.Builder
renderLocalPart (LocalPartDotAtom txt) = shortText txt
renderLocalPart (LocalPartQuoted txt) = "\"" <> escapeQuoted txt <> "\""
  where
    escapeQuoted = M.textUtf8 . T.concatMap escapeChar . ST.toText
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

-- | Render a domain (ASCII/Punycode form).
renderDomain :: Domain -> M.Builder
renderDomain (DomainName ascii _) = shortText ascii
renderDomain (DomainLiteral lit) = shortText lit

-- | Render a domain (Unicode form for display).
renderDomainUnicode :: Domain -> M.Builder
renderDomainUnicode (DomainName ascii mUnicode) =
  case mUnicode of
    Just unicode -> M.textUtf8 unicode
    Nothing -> shortText ascii
renderDomainUnicode (DomainLiteral lit) = shortText lit
