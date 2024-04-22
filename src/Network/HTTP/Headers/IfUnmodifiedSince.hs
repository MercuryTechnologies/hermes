module Network.HTTP.Headers.IfUnmodifiedSince where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hIfUnmodifiedSince)
import Network.HTTP.Headers.Parsing.Util

newtype IfUnmodifiedSince = IfUnmodifiedSince { ifUnmodifiedSince :: UTCTime }
  deriving stock (Eq, Show)

instance KnownHeader IfUnmodifiedSince where
  type ParseFailure IfUnmodifiedSince = String

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser ifUnmodifiedSinceParser header of
      OK ifUnmodifiedSince "" -> Right ifUnmodifiedSince
      OK _ rest -> Left $ "Unconsumed input after parsing If-Unmodified-Since header: " <> show rest
      Fail -> Left "Failed to parse If-Unmodified-Since header"
      Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderIfUnmodifiedSince

  headerName _ = hIfUnmodifiedSince

renderIfUnmodifiedSince :: IfUnmodifiedSince -> M.Builder
renderIfUnmodifiedSince (IfUnmodifiedSince time) = renderDate time

ifUnmodifiedSinceParser :: ParserT st String IfUnmodifiedSince
ifUnmodifiedSinceParser = IfUnmodifiedSince <$> dateParser
