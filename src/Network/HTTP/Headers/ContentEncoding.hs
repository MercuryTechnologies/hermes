module Network.HTTP.Headers.ContentEncoding where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Data.Time.Clock (UTCTime)
import qualified Mason.Builder as M
import Network.HTTP.ContentCoding
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hContentEncoding)
import Network.HTTP.Headers.Parsing.Util

newtype ContentEncoding = ContentEncoding 
  { contentEncoding :: ContentCoding
  }
  deriving stock (Eq, Show)

instance KnownHeader ContentEncoding where
  type ParseFailure ContentEncoding = String
  type Cardinality ContentEncoding = 'ZeroOrOne
  type Direction ContentEncoding = 'Response

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser contentEncodingParser header of
      OK contentEncoding "" -> Right contentEncoding
      OK _ rest -> Left $ "Unconsumed input after parsing Content-Encoding header: " <> show rest
      Fail -> Left "Failed to parse Content-Encoding header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderContentEncoding

  headerName _ = hContentEncoding

contentEncodingParser :: ParserT st String ContentEncoding
contentEncodingParser = ContentEncoding <$> contentCodingParser

renderContentEncoding :: ContentEncoding -> M.Builder
renderContentEncoding (ContentEncoding encoding) = renderContentCoding encoding