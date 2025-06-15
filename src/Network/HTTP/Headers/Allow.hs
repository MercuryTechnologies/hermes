{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Allow
  ( Allow (..)
  , HttpMethod (..)
  , allowParser
  , renderAllow
  ) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAllow)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Standard HTTP methods
data HttpMethod
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  | CustomMethod !ST.ShortText  -- ^ Non-standard method
  deriving stock (Eq, Show)

-- | Allow header containing list of allowed HTTP methods
newtype Allow = Allow { allowedMethods :: [HttpMethod] }
  deriving stock (Eq, Show)

instance KnownHeader Allow where
  type ParseFailure Allow = String
  type Cardinality Allow = 'ZeroOrOne
  type Direction Allow = 'Response

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser allowParser header of
      OK allow "" -> Right allow
      OK _ rest -> Left $ "Unconsumed input after parsing Allow header: " <> show rest
      Fail -> Left "Failed to parse Allow header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderAllow

  headerName _ = hAllow

httpMethodParser :: ParserT st e HttpMethod
httpMethodParser = $(switch [| case _ of
  "GET" -> pure GET
  "HEAD" -> pure HEAD
  "POST" -> pure POST
  "PUT" -> pure PUT
  "DELETE" -> pure DELETE
  "CONNECT" -> pure CONNECT
  "OPTIONS" -> pure OPTIONS
  "TRACE" -> pure TRACE
  "PATCH" -> pure PATCH
  _ -> CustomMethod <$> rfc9110Token
  |])

allowParser :: ParserT st String Allow
allowParser = Allow <$> (httpMethodParser `sepBy` (ows *> $(char ',') *> ows))

renderHttpMethod :: HttpMethod -> M.Builder
renderHttpMethod = \case
  GET -> "GET"
  HEAD -> "HEAD"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"
  CONNECT -> "CONNECT"
  OPTIONS -> "OPTIONS"
  TRACE -> "TRACE"
  PATCH -> "PATCH"
  CustomMethod method -> shortText method

renderAllow :: Allow -> M.Builder
renderAllow (Allow methods) = M.intersperse ", " (map renderHttpMethod methods)
