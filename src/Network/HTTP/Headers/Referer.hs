module Network.HTTP.Headers.Referer
  ( Referer (..)
  , refererParser
  , renderReferer
  ) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReferer)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Referer header value containing the referring URI
newtype Referer = Referer { refererUri :: ST.ShortText }
  deriving stock (Eq, Show)

instance KnownHeader Referer where
  type ParseFailure Referer = String
  type Cardinality Referer = 'ZeroOrOne
  type Direction Referer = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser refererParser header of
      OK referer "" -> Right referer
      OK _ rest -> Left $ "Unconsumed input after parsing Referer header: " <> show rest
      Fail -> Left "Failed to parse Referer header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderReferer

  headerName _ = hReferer

refererParser :: ParserT st String Referer
refererParser = Referer <$> takeRestShortText

renderReferer :: Referer -> M.Builder
renderReferer (Referer uri) = shortText uri
