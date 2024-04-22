module Network.HTTP.Headers.LastModified 
  ( LastModified (..)
  , lastModifiedParser
  , renderLastModified
  ) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hLastModified)
import Network.HTTP.Headers.Parsing.Util

newtype LastModified = LastModified { lastModified :: UTCTime }
  deriving stock (Eq, Show)

instance KnownHeader LastModified where
  type ParseFailure LastModified = String

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser lastModifiedParser header of
      OK lastModified "" -> Right lastModified
      OK _ rest -> Left $ "Unconsumed input after parsing Last-Modified header: " <> show rest
      Fail -> Left "Failed to parse Last-Modified header"
      Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderLastModified

  headerName _ = hLastModified

renderLastModified :: LastModified -> M.Builder
renderLastModified (LastModified time) = renderDate time

lastModifiedParser :: ParserT st String LastModified
lastModifiedParser = LastModified <$> dateParser