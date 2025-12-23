{-# LANGUAGE TemplateHaskell #-}
-- | The Referer header field per RFC 9110 Section 10.1.3.
--
-- The "Referer" header field allows the user agent to specify a
-- URI reference for the resource from which the target URI was
-- obtained (i.e., the "referrer", though the field name is
-- misspelled).
--
-- @
-- Referer = absolute-URI / partial-URI
-- @
module Network.HTTP.Headers.Referer
  ( Referer (..)
  , refererParser
  , renderReferer
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReferer)
import Network.HTTP.Headers.Parsing.Util
import Network.URI

-- | Referer header value containing the referring URI.
--
-- The URI is parsed according to RFC 3986 and supports:
--
-- * Absolute URIs: @https://example.com/path@
-- * Partial URIs: @/path/to/resource@
-- * Internationalized domain names (IDN/Punycode)
--
-- Note: The header name is intentionally misspelled as per the original
-- HTTP specification (should be "Referrer").
newtype Referer = Referer { refererUri :: URI }
  deriving stock (Eq, Show)

instance KnownHeader Referer where
  type ParseFailure Referer = String
  type Cardinality Referer = 'ZeroOrOne
  type Direction Referer = 'Request

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser refererParser header of
      OK referer "" -> Right referer
      OK _ rest -> Left $ "Unconsumed input after parsing Referer header: " <> show rest
      Fail -> Left "Failed to parse Referer header"
      Err err -> Left err

  renderToHeaders _ = M.toStrictByteString . renderReferer

  headerName _ = hReferer

refererParser :: ParserT st String Referer
refererParser = Referer <$> uriReferenceParser

renderReferer :: Referer -> M.Builder
renderReferer (Referer uri) = renderURI uri
