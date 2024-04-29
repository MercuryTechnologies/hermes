module Network.HTTP.Headers.ContentLength where

import Control.Monad.Combinators.NonEmpty
import qualified Data.ByteString as B
import Data.Word (Word64)
import Data.Foldable1
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import FlatParse.Basic
import qualified Mason.Builder as M
import Network.HTTP.Headers.HeaderFieldName
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util
import Network.HTTP.Headers

newtype ContentLength = ContentLength { contentLength :: Word }
  deriving stock (Eq, Show)

instance KnownHeader ContentLength where
  type ParseFailure ContentLength = String
  type Cardinality ContentLength = 'ZeroOrOne
  type Direction ContentLength = 'RequestAndResponse

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser contentLengthParser header of
      OK contentLength "" -> Right contentLength
      OK _ rest -> Left $ "Unconsumed input after parsing Content-Length header: " <> show rest
      Fail -> Left "Failed to parse Content-Length header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderContentLength

  headerName _ = hContentLength

contentLengthParser :: ParserT st String ContentLength
contentLengthParser = ContentLength <$> anyAsciiDecimalWord

renderContentLength :: ContentLength -> M.Builder
renderContentLength (ContentLength len) = M.wordDec len