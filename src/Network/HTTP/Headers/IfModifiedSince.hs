module Network.HTTP.Headers.IfModifiedSince 
  ( renderIfModifiedSince
  , ifModifiedSinceParser
  ) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hIfModifiedSince)
import Network.HTTP.Headers.Parsing.Util

newtype IfModifiedSince = IfModifiedSince { ifModifiedSince :: UTCTime }
  deriving stock (Eq, Show)

instance KnownHeader IfModifiedSince where
  type ParseFailure IfModifiedSince = String
  type Cardinality IfModifiedSince = 'ZeroOrOne
  type Direction IfModifiedSince = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser ifModifiedSinceParser header of
      OK ifModifiedSince "" -> Right ifModifiedSince
      OK _ rest -> Left $ "Unconsumed input after parsing If-Modified-Since header: " <> show rest
      Fail -> Left "Failed to parse If-Modified-Since header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderIfModifiedSince

  headerName _ = hIfModifiedSince

renderIfModifiedSince :: IfModifiedSince -> M.Builder
renderIfModifiedSince (IfModifiedSince time) = renderDate time

ifModifiedSinceParser :: ParserT st String IfModifiedSince
ifModifiedSinceParser = IfModifiedSince <$> dateParser
