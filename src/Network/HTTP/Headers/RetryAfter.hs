module Network.HTTP.Headers.RetryAfter where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hRetryAfter)
import Network.HTTP.Headers.Parsing.Util

data RetryAfter 
  = DelaySeconds Word
  | SpecificTime UTCTime

instance KnownHeader RetryAfter where
  type ParseFailure RetryAfter = String
  type Cardinality RetryAfter = 'ZeroOrOne
  type Direction RetryAfter = 'Response

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser retryAfterParser header of
      OK retryAfter "" -> Right retryAfter
      OK _ rest -> Left $ "Unconsumed input after parsing Retry-After header: " <> show rest
      Fail -> Left "Failed to parse Retry-After header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderRetryAfter

  headerName _ = hRetryAfter

retryAfterParser :: ParserT st String RetryAfter
retryAfterParser =
  (DelaySeconds <$> anyAsciiDecimalWord) <|> 
  (SpecificTime <$> dateParser)

renderRetryAfter :: RetryAfter -> M.Builder
renderRetryAfter = \case
  DelaySeconds seconds -> M.wordDec seconds
  SpecificTime time -> renderDate time
