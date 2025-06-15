{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.TransferEncoding
  ( TransferEncoding (..)
  , transferEncodingParser
  , renderTransferEncoding
  ) where

import Control.Monad.Combinators (sepBy1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTransferEncoding)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Transfer-Encoding is a list of codings such as "chunked", "gzip" etc.
newtype TransferEncoding = TransferEncoding { encodings :: NE.NonEmpty ST.ShortText }
  deriving stock (Eq, Show)

instance KnownHeader TransferEncoding where
  type ParseFailure TransferEncoding = String
  type Cardinality TransferEncoding = 'ZeroOrOne
  type Direction TransferEncoding = 'RequestAndResponse

  parseFromHeaders _ headers = case runParser transferEncodingParser $ NE.head headers of
    OK te "" -> Right te
    OK _ rest -> Left $ "Unconsumed input after parsing Transfer-Encoding header: " <> show rest
    Fail -> Left "Failed to parse Transfer-Encoding header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderTransferEncoding

  headerName _ = hTransferEncoding

transferEncodingParser :: ParserT st String TransferEncoding
transferEncodingParser = do
  firstCoding <- rfc9110Token
  rest <- many (ows *> $(char ',') *> ows *> rfc9110Token)
  pure $ TransferEncoding (firstCoding NE.:| rest)

renderTransferEncoding :: TransferEncoding -> M.Builder
renderTransferEncoding (TransferEncoding codings) =
  M.intersperse ", " $ map shortText $ NE.toList codings
