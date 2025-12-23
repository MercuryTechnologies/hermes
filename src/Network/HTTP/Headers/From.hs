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
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hFrom)
import Network.HTTP.Headers.Parsing.Util
import Network.Mailbox

-- | The From header value containing an email address.
--
-- Per RFC 9110: The From header field contains an Internet email address
-- for a human user who controls the requesting user agent.
--
-- The 'Mailbox' type supports both simple addresses (@user\@example.com@)
-- and addresses with display names (@John Doe \<user\@example.com\>@).
newtype From = From { fromMailbox :: Mailbox }
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

-- | Parse the From header value using the RFC 5322 mailbox parser.
fromParser :: ParserT st String From
fromParser = From <$> mailboxParser

-- | Render the From header value.
renderFrom :: From -> M.Builder
renderFrom (From mailbox) = renderMailbox mailbox
