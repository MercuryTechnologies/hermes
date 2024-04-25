{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Network.HTTP.Headers
  ( KnownHeader (..)
  , HeaderMap
  , headerMapFromList
  , lookupHeader
  , deleteHeader
  , lookupRawHeader
  , insertRawHeader
  , alterRawHeader
  , setHeader
  , setRawHeader
  , HeaderSettings
  , defaultHeaderSettings
  , maxHeaderSize
  ) where

import qualified Data.ByteString as BS
import Data.CaseInsensitive (CI, foldedCase)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Kind
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Typeable
import Network.HTTP.Headers.HeaderFieldName

-- data Header = Header
--   { headerName :: {-# UNPACK #-} !HeaderFieldName
--   , headerValue :: {-# UNPACK #-} !ByteString
--   }

data HeaderMap = HeaderMap 
  { headerMapToMap :: {-# UNPACK #-} !(HashMap HeaderFieldName (NonEmpty BS.ByteString))
  -- ^ Internal invariant: fields are stored in reverse order. They're reversed when
  -- they're looked up.
  --
  -- TODO: Benchmark against using Seq instead of NonEmpty.
  }

lookupRawHeader :: HeaderFieldName -> HeaderMap -> Maybe (NonEmpty BS.ByteString)
lookupRawHeader name (HeaderMap m) = fmap NE.reverse $ name `Map.lookup` m

lookupHeader :: forall a. KnownHeader a => HeaderMap -> Either (ParseFailure a) (Maybe a)
lookupHeader m = case lookupRawHeader (headerName $ Proxy @a) m of
  Nothing -> Right Nothing
  Just entries -> case parseFromHeaders defaultHeaderSettings (NE.reverse entries) of
    Left e -> Left e
    Right a -> Right $ Just a

insertRawHeader :: HeaderFieldName -> BS.ByteString -> HeaderMap -> HeaderMap
insertRawHeader name value (HeaderMap m) = HeaderMap $ Map.alter f name m
  where
    f Nothing = Just $ value NE.:| []
    f (Just (x NE.:| xs)) = Just $ value NE.:| (x : xs)

deleteHeader :: HeaderFieldName -> HeaderMap -> HeaderMap
deleteHeader name (HeaderMap m) = HeaderMap $ Map.delete name m

alterRawHeader :: HeaderFieldName -> (Maybe (NonEmpty BS.ByteString) -> Maybe (NonEmpty BS.ByteString)) -> HeaderMap -> HeaderMap
alterRawHeader name f (HeaderMap m) = HeaderMap $ Map.alter (fmap NE.reverse . f . fmap NE.reverse) name m

setHeader :: forall a. KnownHeader a => a -> HeaderMap -> HeaderMap
setHeader a (HeaderMap m) = case renderToHeaders defaultHeaderSettings a of
  [] -> HeaderMap m
  (x : xs) -> HeaderMap $ Map.insert (headerName (Proxy :: Proxy a)) (NE.reverse (x NE.:| xs)) m

setRawHeader :: HeaderFieldName -> NonEmpty BS.ByteString -> HeaderMap -> HeaderMap
setRawHeader name value (HeaderMap m) = HeaderMap $ Map.insert name (NE.reverse value) m

-- | Build a 'HeaderMap' from a list of headers.
--
-- This is the format used by the 'http-types' and 'wai', so it's useful for
-- integrating with those libraries.
headerMapFromList :: [(CI BS.ByteString, BS.ByteString)] -> HeaderMap
headerMapFromList = foldr f (HeaderMap mempty)
  where
    f (name, value) (HeaderMap m) = HeaderMap $ Map.alter g (unsafeUnknownHeaderFromBytestring $ foldedCase name) m
      where
        g Nothing = Just $ value NE.:| []
        g (Just (x NE.:| xs)) = Just $ value NE.:| (x : xs)

-- addHeader :: Header -> HeaderMap -> HeaderMap
-- addHeader (Header name value) (HeaderMap m) = HeaderMap $ Map.alter f name m
--   where
--     f Nothing = Just $ Reverse $ value :| []
--     f (Just (Reverse (x :| xs))) = Just $ Reverse $ value :| (x : xs)

-- lookupHeader :: HeaderFieldName -> HeaderMap -> Maybe (NonEmpty ByteString)
-- lookupHeader name (HeaderMap m) = fmap getReverse $ Map.lookup name m


-- | Settings for HTTP headers encoding and decoding.
--
-- Many browsers and web servers vary how they handle headers, so
-- this type allows you to define how you want to handle headers
-- in your application to accommodate these differences.
data HeaderSettings = HeaderSettings
  { maxHeaderSize :: Int
  }

-- | Default settings for HTTP headers.
--
-- The default maximum header size is 8192 bytes, which is the maximum size
-- allowed by most web servers (excluding older NGINX builds, which support a max of 4kb per header). 
-- If you need to support larger headers, you can increase this value, but be aware that some servers may reject
-- headers that are too large for security reasons.
defaultHeaderSettings :: HeaderSettings
defaultHeaderSettings = HeaderSettings 8192

-- | A typeclass for parsing and rendering well-known HTTP headers.
--
-- This type class doesn't distinguish between request and response headers
-- because many headers can be used in both requests and responses.
--
-- For headers with different semantics in requests and responses, you should
-- either use newtype wrappers or perform additional validation in your 
-- application logic.
--
-- When defining a new instance, you should generally use a newtype wrapper
-- or brand new data type
class Typeable a => KnownHeader a where
  -- | The type of errors that can occur when parsing a given header.
  --
  -- When possible, it is recommended to use a more specific type than 'String'.
  type ParseFailure a :: Type

  -- | Parse a value from one or more headers with the same header name.
  --
  -- Some headers like 'Set-Cookie' can be split across multiple headers,
  -- so we must accommodate that.
  --
  -- The order of the headers is preserved relative to how they were received.
  --
  -- This function won't be called if the header is not present in the request.
  parseFromHeaders :: HeaderSettings -> NonEmpty BS.ByteString -> Either (ParseFailure a) a
  -- | Similarly to 'parseFromHeaders', we might need to render a value to multiple headers
  -- with the same header name.
  --
  -- If the header can be safely omitted, return '[]'. Otherwise, return a non-empty list of headers.
  renderToHeaders :: HeaderSettings -> a -> [BS.ByteString]
  headerName :: proxy a -> HeaderFieldName
