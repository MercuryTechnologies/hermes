module Network.HTTP.Headers.AcceptEncoding where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Data.Time.Clock (UTCTime)
import qualified Mason.Builder as M
import Network.HTTP.ContentCoding
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hAcceptEncoding)
import Network.HTTP.Headers.Parsing.Util

newtype AcceptEncoding = AcceptEncoding 
  { acceptEncoding :: ContentCoding
  }
  deriving stock (Eq, Show)

instance KnownHeader AcceptEncoding where
  type ParseFailure AcceptEncoding = String
  type Cardinality AcceptEncoding = 'ZeroOrOne
  type Direction AcceptEncoding = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser acceptEncodingParser header of
      OK accept "" -> Right accept
      OK _ rest -> Left $ "Unconsumed input after parsing Accept-Encoding header: " <> show rest
      Fail -> Left "Failed to parse Accept-Encoding header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderAcceptEncoding

  headerName _ = hAcceptEncoding

acceptEncodingParser :: ParserT st String AcceptEncoding
acceptEncodingParser = AcceptEncoding <$> contentCodingParser

renderAcceptEncoding :: AcceptEncoding -> M.Builder
renderAcceptEncoding (AcceptEncoding encoding) = renderContentCoding encoding