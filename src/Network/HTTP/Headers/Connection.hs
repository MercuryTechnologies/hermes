{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Connection
  ( Connection (..)
  , connectionParser
  , renderConnection
  ) where

import Control.Monad.Combinators (sepBy1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hConnection)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Connection header options (e.g., "keep-alive", "close" ...)
newtype Connection = Connection { connectionOptions :: NE.NonEmpty ST.ShortText }
  deriving stock (Eq, Show)

instance KnownHeader Connection where
  type ParseFailure Connection = String
  type Cardinality Connection = 'ZeroOrOne
  type Direction Connection = 'RequestAndResponse

  parseFromHeaders _ headers = case runParser connectionParser $ NE.head headers of
    OK conn "" -> Right conn
    OK _ rest -> Left $ "Unconsumed input after parsing Connection header: " <> show rest
    Fail -> Left "Failed to parse Connection header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderConnection

  headerName _ = hConnection

connectionParser :: ParserT st String Connection
connectionParser = do
  firstOpt <- rfc9110Token
  rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
  pure $ Connection (firstOpt NE.:| rest)

renderConnection :: Connection -> M.Builder
renderConnection (Connection opts) = M.intersperse ", " $ map shortText $ NE.toList opts
