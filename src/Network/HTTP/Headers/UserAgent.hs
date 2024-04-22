{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.UserAgent where

import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Data.Time
import Data.Time.Calendar
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util

data UserAgent = UserAgent
  { firstProduct :: Product
  , remainingUserAgentDefinition :: [Either Comment Product]
  }
  deriving stock (Eq, Show)

data Product = Product
  { productName :: !ShortText
  , productVersion :: !(Maybe ShortText)
  }
  deriving stock (Eq, Show)

instance KnownHeader UserAgent where
  type ParseFailure UserAgent = String

  parseFromHeaders _ headers = case runParser userAgentParser $ NE.head headers of
    OK userAgent "" -> Right userAgent
    OK _ rest -> Left $ "Unconsumed input after parsing User-Agent header: " <> show rest
    Fail -> Left "Failed to parse User-Agent header"
    Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderUserAgent

  headerName _ = hUserAgent

userAgentParser :: ParserT st String UserAgent
userAgentParser = UserAgent <$> productParser <*> many (eitherP commentParser productParser)
  where
    productParser = Product <$> rfc9110Token <*> optional ($(char '/') *> rfc9110Token)
    commentParser = Comment <$> comment

eitherP :: ParserT st e a -> ParserT st e b -> ParserT st e (Either a b)
eitherP l r = (Right <$> r) <|> (Left <$> l)

renderUserAgent :: UserAgent -> M.Builder
renderUserAgent (UserAgent firstProduct remainingUserAgentDefinition) =
  shortText (productName firstProduct) <>
  maybe mempty (\v -> "/" <> shortText v) (productVersion firstProduct) <>
  foldMap (either renderComment renderProduct) remainingUserAgentDefinition
  where
    renderComment (Comment c) = "(" <> M.textUtf8 c <> ")"
    renderProduct (Product n mv) = case mv of
      Nothing -> shortText n
      Just v -> shortText n <> "/" <> shortText v