{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Cookie
  ( Cookie (..)
  , CookiePair (..)
  , cookieParser
  , renderCookie
  ) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCookie)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | A single cookie name-value pair
data CookiePair = CookiePair
  { cookieName :: !ST.ShortText
  , cookieValue :: !ST.ShortText
  } deriving stock (Eq, Show)

-- | Cookie header containing one or more cookie pairs
newtype Cookie = Cookie { cookiePairs :: [CookiePair] }
  deriving stock (Eq, Show)

instance KnownHeader Cookie where
  type ParseFailure Cookie = String
  type Cardinality Cookie = 'ZeroOrOne
  type Direction Cookie = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser cookieParser header of
      OK cookie "" -> Right cookie
      OK _ rest -> Left $ "Unconsumed input after parsing Cookie header: " <> show rest
      Fail -> Left "Failed to parse Cookie header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderCookie

  headerName _ = hCookie

cookiePairParser :: ParserT st String CookiePair
cookiePairParser = do
  name <- rfc9110Token
  $(char '=')
  value <- rfc9110Token <|> quotedString
  pure $ CookiePair name value

cookieParser :: ParserT st String Cookie
cookieParser = Cookie <$> (cookiePairParser `sepBy` (ows *> $(char ';') *> ows))

renderCookiePair :: CookiePair -> M.Builder
renderCookiePair (CookiePair name value) = shortText name <> "=" <> shortText value

renderCookie :: Cookie -> M.Builder
renderCookie (Cookie pairs) = M.intersperse "; " (map renderCookiePair pairs)
