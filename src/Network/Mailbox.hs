{-# LANGUAGE TemplateHaskell #-}
-- | Email address (mailbox) parsing per RFC 5322.
--
-- This module provides types and parsers for Internet email addresses
-- as defined in RFC 5322 (Internet Message Format).
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

-- | A complete mailbox, which may include a display name.
--
-- Examples:
--
-- * @john\@example.com@ (addr-spec only)
-- * @John Doe \<john\@example.com\>@ (with display name)
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
data Domain
  = DomainName !ShortText
  -- ^ A domain name (e.g., @example.com@)
  | DomainLiteral !ShortText
  -- ^ A domain literal (e.g., @[192.168.1.1]@)
  deriving stock (Eq, Show)

-- | Parse a mailbox from a ByteString.
parseMailbox :: ByteString -> Either String Mailbox
parseMailbox bs = case runParser mailboxParser bs of
  OK mailbox "" -> Right mailbox
  OK _ rest -> Left $ "Unconsumed input after parsing mailbox: " <> show rest
  Fail -> Left "Failed to parse mailbox"
  Err err -> Left err

-- | Parse an addr-spec from a ByteString.
parseAddrSpec :: ByteString -> Either String AddrSpec
parseAddrSpec bs = case runParser addrSpecParser bs of
  OK addr "" -> Right addr
  OK _ rest -> Left $ "Unconsumed input after parsing addr-spec: " <> show rest
  Fail -> Left "Failed to parse addr-spec"
  Err err -> Left err

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
domainParser :: ParserT st String Domain
domainParser = domainLiteralParser <|> domainNameParser
  where
    domainNameParser = DomainName <$> domainDotAtomParser
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

-- | Render a mailbox to a Builder.
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

-- | Render an addr-spec to a Builder.
renderAddrSpec :: AddrSpec -> M.Builder
renderAddrSpec (AddrSpec localPart domain) =
  renderLocalPart localPart <> "@" <> renderDomain domain

-- | Render a local part.
renderLocalPart :: LocalPart -> M.Builder
renderLocalPart (LocalPartDotAtom txt) = shortText txt
renderLocalPart (LocalPartQuoted txt) = "\"" <> escapeQuoted txt <> "\""
  where
    escapeQuoted = M.textUtf8 . T.concatMap escapeChar . ST.toText
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

-- | Render a domain.
renderDomain :: Domain -> M.Builder
renderDomain (DomainName txt) = shortText txt
renderDomain (DomainLiteral txt) = shortText txt
