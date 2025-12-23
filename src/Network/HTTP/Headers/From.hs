{-# LANGUAGE TemplateHaskell #-}
-- | The From header field per RFC 9110 Section 10.1.2.
--
-- The "From" header field contains an Internet email address for a
-- human user who controls the requesting user agent.
--
-- @
-- From = mailbox
-- @
--
-- Where mailbox is defined in RFC 5322.
--
-- The From header field is rarely sent by non-robotic user agents.
-- A user agent SHOULD NOT send a From header field without explicit
-- configuration by the user, since that might conflict with the user's
-- privacy interests or their site's security policy.
--
-- A robotic user agent SHOULD send a valid From header field so that
-- the person responsible for running the robot can be contacted if
-- problems occur on servers, such as if the robot is sending excessive,
-- unwanted, or invalid requests.
module Network.HTTP.Headers.From
  ( From (..)
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hFrom)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | The From header value containing an email address.
--
-- Per RFC 9110: The From header field contains an Internet email address
-- for a human user who controls the requesting user agent.
newtype From = From { fromMailbox :: ST.ShortText }
  deriving stock (Eq, Show)

instance KnownHeader From where
  type ParseFailure From = String
  type Cardinality From = 'ZeroOrOne
  type Direction From = 'Request

  parseFromHeaders _ headers = case runParser fromParser $ NE.head headers of
    OK from "" -> Right from
    OK _ rest -> Left $ "Unconsumed input after parsing From header: " <> show rest
    Fail -> Left "Failed to parse From header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderFrom

  headerName _ = hFrom

-- | Parse the From header value.
--
-- Per RFC 5322, a mailbox is:
-- @
-- mailbox = name-addr / addr-spec
-- addr-spec = local-part "@" domain
-- @
--
-- We use a liberal parser that accepts the raw text rather than
-- fully validating the complex RFC 5322 mailbox grammar.
fromParser :: ParserT st String From
fromParser = From <$> mailboxParser

-- | Liberal mailbox parser that accepts common email address formats.
-- We require at least an '@' character for basic email validation.
mailboxParser :: ParserT st String ST.ShortText
mailboxParser = do
  ows
  addr <- parseEmailAddr
  ows
  pure addr
  where
    parseEmailAddr = do
      -- Try to parse angle-bracketed address first (name-addr form)
      -- e.g., "John Doe <john@example.com>"
      (skipToAngleBracket *> angleBracketedAddr) <|> addrSpec

    -- Skip display name until we hit '<'
    skipToAngleBracket = skipMany (skipSatisfyAscii (\c -> c /= '<' && c /= '\r' && c /= '\n'))

    -- Parse <addr-spec>
    angleBracketedAddr = do
      $(char '<')
      addr <- addrSpec
      $(char '>')
      pure addr

    -- Parse a bare addr-spec (local-part@domain)
    addrSpec = do
      result <- shortASCIIFromParser_ $ some (satisfyAscii isAddrChar)
      -- Validate that it contains an '@'
      if ST.any (== '@') result
        then pure result
        else err "Email address must contain '@'"

    -- Characters valid in an email address (liberal interpretation)
    isAddrChar c = c /= ' ' && c /= '\t' && c /= '<' && c /= '>' &&
                   c /= '\r' && c /= '\n' && c /= '(' && c /= ')'

renderFrom :: From -> M.Builder
renderFrom (From mailbox) = shortText mailbox
