{-# LANGUAGE TemplateHaskell #-}
-- | The Server header field per RFC 9110 Section 10.2.4.
--
-- The "Server" header field contains information about the software
-- used by the origin server to handle the request, which is often
-- used by clients to help identify the scope of reported
-- interoperability problems, to work around or tailor requests to
-- avoid particular server limitations, and for analytics regarding
-- server or operating system use.
--
-- @
-- Server = product *( RWS ( product / comment ) )
-- @
module Network.HTTP.Headers.Server
  ( Server (..)
  , Product (..)
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hServer)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | The Server header value.
--
-- Per RFC 9110: An origin server MAY generate a Server header field in its responses.
data Server = Server
  { firstServerProduct :: !Product
  , remainingServerDefinition :: ![Either Comment Product]
  }
  deriving stock (Eq, Show)

-- | A product token with optional version.
--
-- @
-- product = token ["/" product-version]
-- product-version = token
-- @
data Product = Product
  { productName :: !ShortText
  , productVersion :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)

instance KnownHeader Server where
  type ParseFailure Server = String
  type Cardinality Server = 'ZeroOrOne
  type Direction Server = 'Response

  parseFromHeaders _ headers = case runParser serverParser $ NE.head headers of
    OK server "" -> Right server
    OK _ rest -> Left $ "Unconsumed input after parsing Server header: " <> show rest
    Fail -> Left "Failed to parse Server header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderServer

  headerName _ = hServer

serverParser :: ParserT st String Server
serverParser = Server <$> productParser <*> many (rws *> eitherP commentParser productParser)
  where
    productParser = Product <$> rfc9110Token <*> optional ($(char '/') *> rfc9110Token)
    commentParser = Comment <$> comment

eitherP :: ParserT st e a -> ParserT st e b -> ParserT st e (Either a b)
eitherP l r = (Right <$> r) <|> (Left <$> l)

renderServer :: Server -> M.Builder
renderServer (Server firstProduct remaining) =
  renderProduct firstProduct <>
  foldMap renderPart remaining
  where
    renderPart (Left (Comment c)) = " (" <> M.textUtf8 c <> ")"
    renderPart (Right p) = " " <> renderProduct p
    renderProduct (Product n mv) = shortText n <> maybe mempty (\v -> "/" <> shortText v) mv
