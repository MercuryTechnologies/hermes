module Network.HTTP.Headers.Expires 
  ( Expires (..)
  , expiresParser
  , renderExpires
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hExpires)
import Network.HTTP.Headers.Parsing.Util

newtype Expires = Expires { expires :: UTCTime }
  deriving stock (Eq, Show)

instance KnownHeader Expires where
  type ParseFailure Expires = String

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser expiresParser header of
      OK lastModified "" -> Right lastModified
      OK _ rest -> Left $ "Unconsumed input after parsing Expires header: " <> show rest
      Fail -> Left "Failed to parse Expires header"
      Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderExpires

  headerName _ = hExpires

renderExpires :: Expires -> M.Builder
renderExpires (Expires time) = renderDate time

expiresParser :: ParserT st String Expires
expiresParser = Expires <$> dateParser