{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.KeepAlive
  ( KeepAlive (..)
  , keepAliveParser
  , renderKeepAlive
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hKeepAlive)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)
import Control.Monad.Combinators (sepBy)

-- | Keep-Alive header contains parameters like timeout and max.
data KeepAlive = KeepAlive { keepAliveParams :: [(ST.ShortText, ST.ShortText)] }
  deriving stock (Eq, Show)

instance KnownHeader KeepAlive where
  type ParseFailure KeepAlive = String
  type Cardinality KeepAlive = 'ZeroOrOne
  type Direction KeepAlive = 'RequestAndResponse

  parseFromHeaders _ headers = case runParser keepAliveParser $ NE.head headers of
    OK ka "" -> Right ka
    OK _ rest -> Left $ "Unconsumed input after parsing Keep-Alive header: " <> show rest
    Fail -> Left "Failed to parse Keep-Alive header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderKeepAlive

  headerName _ = hKeepAlive

keepAliveParser :: ParserT st String KeepAlive
keepAliveParser = KeepAlive <$> paramParser `sepBy` (ows *> $(char ',') *> ows)
  where
    paramParser = do
      key <- rfc9110Token
      $(char '=')
      val <- rfc9110Token
      pure (key, val)

renderKeepAlive :: KeepAlive -> M.Builder
renderKeepAlive (KeepAlive params) = M.intersperse ", " $ map renderParam params
  where
    renderParam (k, v) = shortText k <> "=" <> shortText v
