{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.IfMatch 
  ( IfMatch (..)
  ) where

import qualified Data.ByteString as B
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers.ETag
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIfMatch)
import Network.HTTP.Headers.Parsing.Util
import FlatParse.Basic
import qualified Mason.Builder as M

-- | The "If-Match" header field makes the request method conditional on 
-- the recipient origin server either having at least one current representation of the target resource, when the field value is "*", or having a current representation of the target resource that has an entity tag matching a member of the 
-- list of entity tags provided in the field value.
data IfMatch
  = IfMatchAnyCurrentRepresentation
  | IfMatchEntityTags (NonEmpty EntityTag)

instance KnownHeader IfMatch where
  type ParseFailure IfMatch = String

  parseFromHeaders _ headers = case runParser ifMatchParser (B.intercalate ", " $ NE.toList headers) of
    OK ifMatch "" -> Right ifMatch
    OK _ rest -> Left $ "Unconsumed input after parsing If-Match header: " <> show rest
    Fail -> Left "Failed to parse If-Match header"
    Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderIfMatch

  headerName _ = hIfMatch

ifMatchParser :: ParserT st e IfMatch
ifMatchParser = 
  ($(string "*") *> pure IfMatchAnyCurrentRepresentation) <|> 
  (IfMatchEntityTags <$> entityTagParser `sepBy1` (ows *> $(char ',') *> ows))

renderIfMatch :: IfMatch -> M.Builder
renderIfMatch = \case
  IfMatchAnyCurrentRepresentation -> "*"
  IfMatchEntityTags tags -> M.intersperse ", " $ renderEntityTag <$> tags
