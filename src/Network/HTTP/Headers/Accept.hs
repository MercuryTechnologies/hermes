module Network.HTTP.Headers.Accept 
  ( Accept (..)
  , acceptParser
  , renderAccept
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.ContentNegotiation
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hAccept)
import Network.HTTP.Headers.Parsing.Util

newtype Accept = Accept { accept :: [WeightedMediaRange] }

instance KnownHeader Accept where
  type ParseFailure Accept = String

  parseFromHeaders _ headers = case runParser acceptParser $ NE.head headers of
    OK accept "" -> Right accept
    OK _ rest -> Left $ "Unconsumed input after parsing Accept header: " <> show rest
    Fail -> Left "Failed to parse Accept header"
    Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderAccept

  headerName _ = hAccept

acceptParser :: ParserT st String Accept
acceptParser = Accept <$> weightedMediaRangesParser

renderAccept :: Accept -> M.Builder
renderAccept (Accept mediaRanges) = renderWeightedMediaRanges mediaRanges

