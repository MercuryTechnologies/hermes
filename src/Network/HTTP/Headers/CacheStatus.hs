module Network.HTTP.Headers.CacheStatus 
  ( CacheStatus (..)
  ) where

import Control.Monad.Permutations
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import qualified Mason.Builder as M
import Network.HTTP.Headers (KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hCacheStatus)
import Network.HTTP.Headers.Parsing.Util

{-
To aid debugging (both by humans and automated tools), HTTP caches often append header fields to a response explaining how they handled the request. Unfortunately, the semantics of these header fields are often unclear, and both the semantics and syntax used vary between implementations.

This specification defines a new HTTP response header field, "Cache-Status", for this purpose with standardized syntax and semantics.
-}
newtype CacheStatus = CacheStatus 
  { cacheStatus :: [CacheStatusEntry]
  }
  deriving stock (Eq, Show)

data CacheStatusEntry = CacheStatusEntry
  { cacheStatusIdentifier :: {-# UNPACK #-} !ShortText
  , cacheStatusHitOrForward :: !(Either Hit ForwardInfo)
  , cacheStatusTtl :: !(Maybe Int)
  , cacheStatusKey :: !(Maybe ShortText)
  , cacheStatusDetail :: !(Maybe ShortText)
  , cacheStatusUnknownParameters :: ![(ShortText, Maybe ItemValue)]
  } deriving stock (Eq, Show)

-- | When present, indicates that the request went forward towards the origin
--
-- The most specific reason known to the cache SHOULD be used, to the extent that it is possible to implement.
data Forward
  = Bypass
  -- ^ The cache was configured to not handle this request.
  | Method
  -- ^ The request method's semantics require the request to be forwarded.
  | UriMiss
  -- ^ The cache did not contain any responses that matched the request URI.
  | VaryMiss
  -- ^ The cache contained a response that matched the request URI, but it could not select a response based 
  -- upon this request's header fields and stored Vary header fields.
  | Miss
  -- ^ The cache did not contain any responses that could be used to satisfy this request 
  -- (to be used when an implementation cannot distinguish between uri-miss and vary-miss).
  | Request
  -- ^ The cache was able to select a fresh response for the request, but the request's 
  -- semantics (e.g., Cache-Control request directives) did not allow its use.
  | Stale
  -- ^ The cache was able to select a response for the request, but it was stale.
  | Partial
  -- ^ The cache was able to select a partial response for the request, but it did not contain all of the requested ranges (or the request was for the complete response).
  | UnknownForward !ShortText
  deriving stock (Eq, Show, Ord)

data ForwardInfo = ForwardInfo
  { forwardType :: !Forward
  , forwardStatus :: !(Maybe Int)
  , forwardStored :: !Bool
  , forwardCollapsed :: !Bool
  } deriving stock (Eq, Show)

data Hit = Hit
  deriving stock (Eq, Show)
