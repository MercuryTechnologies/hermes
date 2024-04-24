module Network.HTTP.Headers.PingTo where

import qualified Data.List.NonEmpty as NE
import FlatParse.Basic
import qualified Mason.Builder as M
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hPingTo)

newtype PingTo = PingTo { pingTo :: String }
  deriving stock (Eq, Show)

instance KnownHeader PingTo where
  type ParseFailure PingTo = String

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser pingToParser header of
      OK pingTo "" -> Right pingTo
      OK _ rest -> Left $ "Unconsumed input after parsing Ping-To header: " <> show rest
      Fail -> Left "Failed to parse Ping-To header"
      Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderPingTo

  headerName _ = hPingTo

pingToParser :: ParserT st String PingTo
pingToParser = PingTo <$> takeRestString

renderPingTo :: PingTo -> M.Builder
renderPingTo (PingTo str) = M.string8 str
