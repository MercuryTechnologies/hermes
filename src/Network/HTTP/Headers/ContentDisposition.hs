{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.ContentDisposition
  ( ContentDisposition (..)
  , contentDispositionParser
  , renderContentDisposition
  ) where

import Control.Monad.Combinators (sepBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentDisposition)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Content-Disposition value consists of a disposition type and optional parameters
data ContentDisposition = ContentDisposition
  { dispositionType :: !ST.ShortText
  , dispositionParams :: ![(ST.ShortText, ST.ShortText)]
  } deriving stock (Eq, Show)

instance KnownHeader ContentDisposition where
  type ParseFailure ContentDisposition = String
  type Cardinality ContentDisposition = 'ZeroOrOne
  type Direction ContentDisposition = 'Response

  parseFromHeaders _ headers = case runParser contentDispositionParser $ NE.head headers of
    OK cd "" -> Right cd
    OK _ rest -> Left $ "Unconsumed input after parsing Content-Disposition header: " <> show rest
    Fail -> Left "Failed to parse Content-Disposition header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderContentDisposition

  headerName _ = hContentDisposition

contentDispositionParser :: ParserT st String ContentDisposition
contentDispositionParser = do
  dtype <- rfc9110Token
  params <- many (ows *> $(char ';') *> ows *> param)
  pure $ ContentDisposition dtype params
  where
    param = (,) <$> rfc9110Token <*> ($(char '=') *> (rfc9110Token <|> quotedString))

renderContentDisposition :: ContentDisposition -> M.Builder
renderContentDisposition (ContentDisposition dtype params) =
  shortText dtype <> foldMap renderParam params
  where
    renderParam (k, v) = M.char7 ';' <> shortText k <> M.char7 '=' <> shortText v
