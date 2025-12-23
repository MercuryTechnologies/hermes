{-# LANGUAGE TemplateHaskell #-}
-- | The If-None-Match header field per RFC 9110 Section 13.1.2.
--
-- The "If-None-Match" header field makes the request method conditional
-- on a recipient cache or origin server either not having any current
-- representation of the target resource, when the field value is "*",
-- or having a selected representation with an entity tag that does not
-- match any of those listed in the field value.
--
-- @
-- If-None-Match = "*" / #entity-tag
-- @
--
-- A recipient MUST use the weak comparison function when comparing
-- entity tags for If-None-Match (Section 8.8.3.2), since weak entity
-- tags can be used for cache validation even if there have been changes
-- to the representation data.
module Network.HTTP.Headers.IfNoneMatch
  ( IfNoneMatch (..)
  ) where

import qualified Data.ByteString as B
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers.ETag
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hIfNoneMatch)
import Network.HTTP.Headers.Parsing.Util
import FlatParse.Basic
import qualified Mason.Builder as M

-- | The "If-None-Match" header field makes the request method conditional
-- on a recipient cache or origin server either not having any current
-- representation of the target resource, when the field value is "*",
-- or having a selected representation with an entity tag that does not
-- match any of those listed in the field value.
data IfNoneMatch
  = IfNoneMatchAnyRepresentation
  -- ^ The "*" value matches any current representation of the resource.
  | IfNoneMatchEntityTags (NonEmpty EntityTag)
  -- ^ A list of entity tags to compare against.
  deriving stock (Eq, Show)

instance KnownHeader IfNoneMatch where
  type ParseFailure IfNoneMatch = String
  type Cardinality IfNoneMatch = 'ZeroOrOne
  type Direction IfNoneMatch = 'Request

  parseFromHeaders _ headers = case runParser ifNoneMatchParser (B.intercalate ", " $ NE.toList headers) of
    OK ifNoneMatch "" -> Right ifNoneMatch
    OK _ rest -> Left $ "Unconsumed input after parsing If-None-Match header: " <> show rest
    Fail -> Left "Failed to parse If-None-Match header"
    Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderIfNoneMatch

  headerName _ = hIfNoneMatch

ifNoneMatchParser :: ParserT st e IfNoneMatch
ifNoneMatchParser =
  ($(string "*") *> pure IfNoneMatchAnyRepresentation) <|>
  (IfNoneMatchEntityTags <$> entityTagParser `sepBy1` (ows *> $(char ',') *> ows))

renderIfNoneMatch :: IfNoneMatch -> M.Builder
renderIfNoneMatch = \case
  IfNoneMatchAnyRepresentation -> "*"
  IfNoneMatchEntityTags tags -> M.intersperse ", " $ renderEntityTag <$> tags
