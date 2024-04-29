module Network.HTTP.Headers.Sunset where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hSunset)
import Network.HTTP.Headers.Parsing.Util

newtype Sunset = Sunset { sunsetDate :: UTCTime }
  deriving stock (Eq, Show)

instance KnownHeader Sunset where
  type ParseFailure Sunset = String
  type Cardinality Sunset = 'ZeroOrOne
  type Direction Sunset = 'Response

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser sunsetParser header of
      OK sunset "" -> Right sunset
      OK _ rest -> Left $ "Unconsumed input after parsing Sunset header: " <> show rest
      Fail -> Left "Failed to parse Sunset header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderSunset

  headerName _ = hSunset

sunsetParser :: ParserT st String Sunset
sunsetParser = Sunset <$> dateParser

renderSunset :: Sunset -> M.Builder
renderSunset (Sunset time) = renderDate time
