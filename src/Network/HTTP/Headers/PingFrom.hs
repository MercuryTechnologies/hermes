module Network.HTTP.Headers.PingFrom where

import qualified Data.List.NonEmpty as NE
import FlatParse.Basic
import qualified Mason.Builder as M
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hPingFrom)

newtype PingFrom = PingFrom { pingFrom :: String }
  deriving stock (Eq, Show)

instance KnownHeader PingFrom where
  type ParseFailure PingFrom = String

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser pingFromParser header of
      OK pingFrom "" -> Right pingFrom
      OK _ rest -> Left $ "Unconsumed input after parsing Ping-From header: " <> show rest
      Fail -> Left "Failed to parse Ping-From header"
      Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderPingFrom

  headerName _ = hPingFrom

pingFromParser :: ParserT st String PingFrom
pingFromParser = PingFrom <$> takeRestString

renderPingFrom :: PingFrom -> M.Builder
renderPingFrom (PingFrom str) = M.string8 str
