{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Host
  ( Host (..)
  , hostParser
  , renderHost
  ) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hHost)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Host header value representing a hostname and optional port
data Host = Host
  { hostName :: !ST.ShortText  -- ^ The hostname (e.g. "example.com")
  , hostPort :: !(Maybe Word16)  -- ^ Optional port number
  } deriving stock (Eq, Show)

instance KnownHeader Host where
  type ParseFailure Host = String
  type Cardinality Host = 'One
  type Direction Host = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser hostParser header of
      OK host "" -> Right host
      OK _ rest -> Left $ "Unconsumed input after parsing Host header: " <> show rest
      Fail -> Left "Failed to parse Host header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderHost

  headerName _ = hHost

hostParser :: ParserT st String Host
hostParser = do
  hostname <- rfc9110Token
  port <- optional $ do
    $(char ':')
    anyAsciiDecimalWord
  pure $ Host hostname (fromIntegral <$> port)

renderHost :: Host -> M.Builder
renderHost (Host hostname mPort) =
  shortText hostname <> maybe mempty (\port -> M.char7 ':' <> M.word16Dec port) mPort
