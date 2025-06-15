{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Origin
  ( Origin (..)
  , OriginValue (..)
  , originParser
  , renderOrigin
  ) where

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word16)
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hOrigin)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Origin value can be null or a specific origin
data OriginValue
  = OriginNull
  | Origin
    { originScheme :: !ST.ShortText  -- ^ e.g. "https"
    , originHost :: !ST.ShortText    -- ^ e.g. "example.com"
    , originPort :: !(Maybe Word16)  -- ^ Optional port
    }
  deriving stock (Eq, Show)

-- | Origin header containing origin information
newtype Origin = OriginHeader { originValue :: OriginValue }
  deriving stock (Eq, Show)

instance KnownHeader Origin where
  type ParseFailure Origin = String
  type Cardinality Origin = 'ZeroOrOne
  type Direction Origin = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser originParser header of
      OK origin "" -> Right origin
      OK _ rest -> Left $ "Unconsumed input after parsing Origin header: " <> show rest
      Fail -> Left "Failed to parse Origin header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderOrigin

  headerName _ = hOrigin

originParser :: ParserT st String Origin
originParser = OriginHeader <$> originValueParser
  where
    originValueParser = nullOrigin <|> specificOrigin
    nullOrigin = $(string "null") *> pure OriginNull
    specificOrigin = do
      scheme <- rfc9110Token
      $(string "://")
      host <- rfc9110Token
      port <- optional $ do
        $(char ':')
        anyAsciiDecimalWord
      pure $ Origin scheme host (fromIntegral <$> port)

renderOrigin :: Origin -> M.Builder
renderOrigin (OriginHeader OriginNull) = "null"
renderOrigin (OriginHeader (Origin scheme host mPort)) =
  shortText scheme <> "://" <> shortText host <>
  maybe mempty (\port -> M.char7 ':' <> M.word16Dec port) mPort
