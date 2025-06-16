{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Network.H2O.Types
-- Description : Core types for H2O HTTP server bindings
-- Copyright   : (c) 2024 Mercury Technologies
-- License     : BSD-3-Clause
-- Maintainer  : ian@mercury.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides low-level Haskell bindings to the H2O HTTP server
-- library. H2O is a high-performance HTTP server with support for HTTP/1.x,
-- HTTP/2, and WebSocket protocols.
--
-- These bindings are designed to be comprehensive and type-safe while
-- maintaining performance. All foreign functions are marked as unsafe
-- unless they may block or call back into Haskell code.

module Network.H2O.Types
  ( -- * Core Types
    H2OContext
  , H2OGlobalConf
  , H2OHostConf
  , H2OPathConf
  , H2OHandler
  , H2ORequest
  , H2OResponse
  , H2OSocket
  , H2OAcceptCtx
  , H2OEvLoop
  , H2OGenerator
  , H2OMemPool

    -- * Basic Data Types
  , H2OIovec(..)
  , H2OStringRef(..)
  , H2OVector(..)

    -- * HTTP Status Codes
  , H2OStatus
  , pattern H2O_STATUS_OK
  , pattern H2O_STATUS_NOT_FOUND
  , pattern H2O_STATUS_INTERNAL_SERVER_ERROR
  , pattern H2O_STATUS_BAD_REQUEST

    -- * HTTP Methods
  , H2OMethod
  , pattern H2O_METHOD_GET
  , pattern H2O_METHOD_POST
  , pattern H2O_METHOD_PUT
  , pattern H2O_METHOD_DELETE
  , pattern H2O_METHOD_HEAD
  , pattern H2O_METHOD_OPTIONS

    -- * Socket Flags
  , H2OSocketFlag
  , pattern H2O_SOCKET_FLAG_DONT_READ

    -- * Send State
  , H2OSendState
  , pattern H2O_SEND_STATE_IN_PROGRESS
  , pattern H2O_SEND_STATE_FINAL

    -- * Utility Functions
  , withH2OString
  , h2oStringToByteString
  , byteStringToH2OString
  ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

#include <h2o.h>

-- | Opaque type representing H2O context
data H2OContext

-- | Opaque type representing H2O global configuration
data H2OGlobalConf

-- | Opaque type representing H2O host configuration
data H2OHostConf

-- | Opaque type representing H2O path configuration
data H2OPathConf

-- | Opaque type representing H2O handler
data H2OHandler

-- | Opaque type representing H2O request
data H2ORequest

-- | Opaque type representing H2O response
data H2OResponse

-- | Opaque type representing H2O socket
data H2OSocket

-- | Opaque type representing H2O accept context
data H2OAcceptCtx

-- | Opaque type representing H2O event loop
data H2OEvLoop

-- | Opaque type representing H2O generator
data H2OGenerator

-- | Opaque type representing H2O memory pool
data H2OMemPool

-- | H2O I/O vector structure
data H2OIovec = H2OIovec
  { h2oIovecBase :: !(Ptr Word8)  -- ^ Pointer to data
  , h2oIovecLen  :: !CSize        -- ^ Length of data
  } deriving (Show, Eq)

instance Storable H2OIovec where
  sizeOf _ = (#size h2o_iovec_t)
  alignment _ = (#alignment h2o_iovec_t)
  peek ptr = H2OIovec
    <$> (#peek h2o_iovec_t, base) ptr
    <*> (#peek h2o_iovec_t, len) ptr
  poke ptr (H2OIovec base len) = do
    (#poke h2o_iovec_t, base) ptr base
    (#poke h2o_iovec_t, len) ptr len

-- | H2O string reference structure (pointer + length)
data H2OStringRef = H2OStringRef
  { h2oStringBase :: !(Ptr CChar)  -- ^ Pointer to string data
  , h2oStringLen  :: !CSize        -- ^ Length of string
  } deriving (Show, Eq)

instance Storable H2OStringRef where
  sizeOf _ = (#size h2o_iovec_t)
  alignment _ = (#alignment h2o_iovec_t)
  peek ptr = H2OStringRef
    <$> (#peek h2o_iovec_t, base) ptr
    <*> (#peek h2o_iovec_t, len) ptr
  poke ptr (H2OStringRef base len) = do
    (#poke h2o_iovec_t, base) ptr base
    (#poke h2o_iovec_t, len) ptr len

-- | H2O vector structure (dynamic array)
data H2OVector a = H2OVector
  { h2oVectorEntries :: !(Ptr a)  -- ^ Pointer to entries
  , h2oVectorSize    :: !CSize    -- ^ Number of entries
  , h2oVectorCapacity :: !CSize   -- ^ Allocated capacity
  } deriving (Show, Eq)

instance Storable a => Storable (H2OVector a) where
  sizeOf _ = (#size h2o_vector_t)
  alignment _ = (#alignment h2o_vector_t)
  peek ptr = H2OVector
    <$> (#peek h2o_vector_t, entries) ptr
    <*> (#peek h2o_vector_t, size) ptr
    <*> (#peek h2o_vector_t, capacity) ptr
  poke ptr (H2OVector entries size capacity) = do
    (#poke h2o_vector_t, entries) ptr entries
    (#poke h2o_vector_t, size) ptr size
    (#poke h2o_vector_t, capacity) ptr capacity

-- | HTTP status code
newtype H2OStatus = H2OStatus CInt
  deriving (Show, Eq, Ord, Storable, Num)

pattern H2O_STATUS_OK :: H2OStatus
pattern H2O_STATUS_OK = H2OStatus 200

pattern H2O_STATUS_BAD_REQUEST :: H2OStatus
pattern H2O_STATUS_BAD_REQUEST = H2OStatus 400

pattern H2O_STATUS_NOT_FOUND :: H2OStatus
pattern H2O_STATUS_NOT_FOUND = H2OStatus 404

pattern H2O_STATUS_INTERNAL_SERVER_ERROR :: H2OStatus
pattern H2O_STATUS_INTERNAL_SERVER_ERROR = H2OStatus 500

-- | HTTP method
newtype H2OMethod = H2OMethod H2OStringRef
  deriving (Show, Eq, Storable)

pattern H2O_METHOD_GET :: H2OStringRef
pattern H2O_METHOD_GET = H2OStringRef nullPtr 3

pattern H2O_METHOD_POST :: H2OStringRef
pattern H2O_METHOD_POST = H2OStringRef nullPtr 4

pattern H2O_METHOD_PUT :: H2OStringRef
pattern H2O_METHOD_PUT = H2OStringRef nullPtr 3

pattern H2O_METHOD_DELETE :: H2OStringRef
pattern H2O_METHOD_DELETE = H2OStringRef nullPtr 6

pattern H2O_METHOD_HEAD :: H2OStringRef
pattern H2O_METHOD_HEAD = H2OStringRef nullPtr 4

pattern H2O_METHOD_OPTIONS :: H2OStringRef
pattern H2O_METHOD_OPTIONS = H2OStringRef nullPtr 7

-- | Socket flags
newtype H2OSocketFlag = H2OSocketFlag CInt
  deriving (Show, Eq, Ord, Storable, Num, Bits)

pattern H2O_SOCKET_FLAG_DONT_READ :: H2OSocketFlag
pattern H2O_SOCKET_FLAG_DONT_READ = H2OSocketFlag 1

-- | Send state for generators
newtype H2OSendState = H2OSendState CInt
  deriving (Show, Eq, Ord, Storable, Num)

pattern H2O_SEND_STATE_IN_PROGRESS :: H2OSendState
pattern H2O_SEND_STATE_IN_PROGRESS = H2OSendState 0

pattern H2O_SEND_STATE_FINAL :: H2OSendState
pattern H2O_SEND_STATE_FINAL = H2OSendState 1

-- | Convert a ByteString to H2O string format with automatic memory management
withH2OString :: ByteString -> (H2OStringRef -> IO a) -> IO a
withH2OString bs action = 
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    action (H2OStringRef ptr (fromIntegral len))

-- | Convert H2O string to ByteString (copies data)
h2oStringToByteString :: H2OStringRef -> IO ByteString
h2oStringToByteString (H2OStringRef ptr len) = 
  BS.packCStringLen (ptr, fromIntegral len)

-- | Convert ByteString to H2O string (copies data to H2O memory pool)
byteStringToH2OString :: Ptr H2OMemPool -> ByteString -> IO H2OStringRef
byteStringToH2OString pool bs = do
  let len = BS.length bs
  ptr <- h2o_mem_alloc_pool pool (fromIntegral len)
  BSU.unsafeUseAsCString bs $ \cstr -> do
    copyBytes ptr cstr len
    return (H2OStringRef ptr (fromIntegral len))

-- Foreign imports for memory management
foreign import ccall unsafe "h2o_mem_alloc_pool"
  h2o_mem_alloc_pool :: Ptr H2OMemPool -> CSize -> IO (Ptr CChar)