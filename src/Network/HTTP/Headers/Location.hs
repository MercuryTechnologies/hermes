{-# LANGUAGE TemplateHaskell #-}
-- | The Location header field per RFC 9110 Section 10.2.2.
--
-- The "Location" header field is used in some responses to refer to a
-- specific resource in relation to the response. The type of
-- relationship is defined by the combination of request method and
-- status code semantics.
--
-- @
-- Location = URI-reference
-- @
module Network.HTTP.Headers.Location
  ( Location (..)
  , locationParser
  , renderLocation
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hLocation)
import Network.HTTP.Headers.Parsing.Util
import Network.URI

-- | Location header value containing a URI-reference.
--
-- The URI is parsed according to RFC 3986 and supports:
--
-- * Absolute URIs: @https://example.com/path@
-- * Relative references: @/path/to/resource@
-- * Internationalized domain names (IDN/Punycode)
newtype Location = Location { locationUri :: URI }
  deriving stock (Eq, Show)

instance KnownHeader Location where
  type ParseFailure Location = String
  type Cardinality Location = 'ZeroOrOne
  type Direction Location = 'Response

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser locationParser header of
      OK location "" -> Right location
      OK _ rest -> Left $ "Unconsumed input after parsing Location header: " <> show rest
      Fail -> Left "Failed to parse Location header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderLocation

  headerName _ = hLocation

locationParser :: ParserT st String Location
locationParser = Location <$> uriReferenceParser

renderLocation :: Location -> M.Builder
renderLocation (Location uri) = renderURI uri
