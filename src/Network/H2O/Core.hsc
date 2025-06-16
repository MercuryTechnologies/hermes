{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Network.H2O.Core
-- Description : Core H2O HTTP server bindings
-- Copyright   : (c) 2024 Mercury Technologies
-- License     : BSD-3-Clause
-- Maintainer  : ian@mercury.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides the core functionality for H2O HTTP server,
-- including configuration management, context initialization, and
-- event loop operations.

module Network.H2O.Core
  ( -- * Configuration Management
    h2oConfigInit
  , h2oConfigRegisterHost
  , h2oConfigRegisterPath
  , h2oConfigDispose

    -- * Context Management
  , h2oContextInit
  , h2oContextDispose

    -- * Handler Management
  , h2oCreateHandler
  , h2oRegisterRequestHandler

    -- * Event Loop Operations
  , h2oEvloopCreate
  , h2oEvloopDestroy
  , h2oEvloopRun

    -- * Socket Operations
  , h2oEvloopSocketCreate
  , h2oSocketReadStart
  , h2oSocketClose
  , h2oAccept

    -- * Request/Response Operations
  , h2oSendInline
  , h2oSend
  , h2oStartResponse
  , h2oAddHeader

    -- * Memory Pool Operations
  , h2oMemAllocPool
  , h2oMemAllocShared

    -- * String Operations
  , h2oStrDup
  , h2oMemIs

    -- * Utility Functions
  , h2oIovecInit
  , h2oVectorReserve

    -- * Callback Types
  , H2OHandlerCallback
  , H2OSocketCallback
  , H2OGeneratorProceed
  , H2OGeneratorStop

    -- * Error Handling
  , H2OResult
  , pattern H2O_OK
  , pattern H2O_ERROR
  , h2oResultToEither
  ) where

import Network.H2O.Types
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

#include <h2o.h>

-- | Result type for H2O operations
newtype H2OResult = H2OResult CInt
  deriving (Show, Eq, Ord, Storable, Num)

pattern H2O_OK :: H2OResult
pattern H2O_OK = H2OResult 0

pattern H2O_ERROR :: H2OResult
pattern H2O_ERROR = H2OResult (-1)

-- | Convert H2O result to Either
h2oResultToEither :: H2OResult -> String -> Either String ()
h2oResultToEither H2O_OK _ = Right ()
h2oResultToEither _ err = Left err

-- | Callback type for request handlers
-- Returns 0 on success, -1 to pass to next handler
type H2OHandlerCallback = Ptr H2OHandler -> Ptr H2ORequest -> IO CInt

-- | Callback type for socket events
type H2OSocketCallback = Ptr H2OSocket -> CString -> IO ()

-- | Generator proceed callback type
type H2OGeneratorProceed = Ptr H2OGenerator -> Ptr H2ORequest -> IO ()

-- | Generator stop callback type
type H2OGeneratorStop = Ptr H2OGenerator -> Ptr H2ORequest -> IO ()

-- Configuration Management

-- | Initialize H2O global configuration
foreign import ccall unsafe "h2o_config_init"
  h2oConfigInit :: Ptr H2OGlobalConf -> IO ()

-- | Register a host in the configuration
foreign import ccall unsafe "h2o_config_register_host"
  h2oConfigRegisterHost :: Ptr H2OGlobalConf -> H2OStringRef -> CUShort -> IO (Ptr H2OHostConf)

-- | Register a path for a host
foreign import ccall unsafe "h2o_config_register_path"
  h2oConfigRegisterPath :: Ptr H2OHostConf -> CString -> CInt -> IO (Ptr H2OPathConf)

-- | Dispose of H2O configuration
foreign import ccall unsafe "h2o_config_dispose"
  h2oConfigDispose :: Ptr H2OGlobalConf -> IO ()

-- Context Management

-- | Initialize H2O context
foreign import ccall unsafe "h2o_context_init"
  h2oContextInit :: Ptr H2OContext -> Ptr H2OEvLoop -> Ptr H2OGlobalConf -> IO ()

-- | Dispose of H2O context
foreign import ccall unsafe "h2o_context_dispose"
  h2oContextDispose :: Ptr H2OContext -> IO ()

-- Handler Management

-- | Create a new handler for a path
foreign import ccall unsafe "h2o_create_handler"
  h2oCreateHandler :: Ptr H2OPathConf -> CSize -> IO (Ptr H2OHandler)

-- | Register a request handler (Haskell helper function)
h2oRegisterRequestHandler :: Ptr H2OPathConf -> H2OHandlerCallback -> IO (Ptr H2OHandler)
h2oRegisterRequestHandler pathConf callback = do
  handler <- h2oCreateHandler pathConf (#size h2o_handler_t)
  callbackPtr <- wrapH2OHandlerCallback callback
  (#poke h2o_handler_t, on_req) handler callbackPtr
  return handler

foreign import ccall "wrapper"
  wrapH2OHandlerCallback :: H2OHandlerCallback -> IO (FunPtr H2OHandlerCallback)

-- Event Loop Operations

-- | Create new event loop
foreign import ccall unsafe "h2o_evloop_create"
  h2oEvloopCreate :: IO (Ptr H2OEvLoop)

-- | Destroy event loop
foreign import ccall unsafe "h2o_evloop_destroy"
  h2oEvloopDestroy :: Ptr H2OEvLoop -> IO ()

-- | Run event loop (blocking call)
foreign import ccall safe "h2o_evloop_run"
  h2oEvloopRun :: Ptr H2OEvLoop -> CInt -> IO CInt

-- Socket Operations

-- | Create socket attached to event loop
foreign import ccall unsafe "h2o_evloop_socket_create"
  h2oEvloopSocketCreate :: Ptr H2OEvLoop -> CInt -> H2OSocketFlag -> IO (Ptr H2OSocket)

-- | Start reading from socket
foreign import ccall unsafe "h2o_socket_read_start"
  h2oSocketReadStart :: Ptr H2OSocket -> FunPtr H2OSocketCallback -> IO ()

-- | Close socket
foreign import ccall unsafe "h2o_socket_close"
  h2oSocketClose :: Ptr H2OSocket -> IO ()

-- | Accept new connection
foreign import ccall unsafe "h2o_accept"
  h2oAccept :: Ptr H2OAcceptCtx -> Ptr H2OSocket -> IO ()

-- Request/Response Operations

-- | Send inline response data
foreign import ccall unsafe "h2o_send_inline"
  h2oSendInline :: Ptr H2ORequest -> CString -> CSize -> IO ()

-- | Send response data using iovec
foreign import ccall unsafe "h2o_send"
  h2oSend :: Ptr H2ORequest -> Ptr H2OIovec -> CSize -> H2OSendState -> IO ()

-- | Start response with generator
foreign import ccall unsafe "h2o_start_response"
  h2oStartResponse :: Ptr H2ORequest -> Ptr H2OGenerator -> IO ()

-- | Add header to response
foreign import ccall unsafe "h2o_add_header"
  h2oAddHeader :: Ptr H2OMemPool -> Ptr (H2OVector H2OIovec) -> CString -> CString -> CString -> CSize -> IO ()

-- Memory Pool Operations

-- | Allocate memory from pool
foreign import ccall unsafe "h2o_mem_alloc_pool"
  h2oMemAllocPool :: Ptr H2OMemPool -> CSize -> IO (Ptr a)

-- | Allocate shared memory with cleanup callback
foreign import ccall unsafe "h2o_mem_alloc_shared"
  h2oMemAllocShared :: Ptr H2OMemPool -> CSize -> FunPtr (Ptr a -> IO ()) -> IO (Ptr a)

-- String Operations

-- | Duplicate string using memory pool
foreign import ccall unsafe "h2o_strdup"
  h2oStrDup :: Ptr H2OMemPool -> CString -> CSize -> IO H2OStringRef

-- | Compare memory regions
foreign import ccall unsafe "h2o_memis"
  h2oMemIs :: CString -> CSize -> CString -> CSize -> IO CInt

-- Utility Functions

-- | Initialize H2O iovec
h2oIovecInit :: Ptr a -> CSize -> H2OIovec
h2oIovecInit ptr len = H2OIovec (castPtr ptr) len

-- | Reserve space in vector
foreign import ccall unsafe "h2o_vector_reserve"
  h2oVectorReserve :: Ptr H2OMemPool -> Ptr (H2OVector a) -> CSize -> IO (Ptr a)

-- Helper function to send ByteString as inline response
h2oSendByteStringInline :: Ptr H2ORequest -> ByteString -> IO ()
h2oSendByteStringInline req bs = 
  BS.useAsCStringLen bs $ \(ptr, len) ->
    h2oSendInline req ptr (fromIntegral len)

-- Helper function to send ByteString using iovec
h2oSendByteString :: Ptr H2ORequest -> ByteString -> H2OSendState -> IO ()
h2oSendByteString req bs state = 
  BS.useAsCStringLen bs $ \(ptr, len) -> do
    let iovec = h2oIovecInit ptr (fromIntegral len)
    with iovec $ \iovecPtr ->
      h2oSend req iovecPtr 1 state

-- Helper function to compare H2O strings with ByteString
h2oMemIsByteString :: H2OStringRef -> ByteString -> IO Bool
h2oMemIsByteString (H2OStringRef ptr1 len1) bs =
  BS.useAsCStringLen bs $ \(ptr2, len2) -> do
    result <- h2oMemIs ptr1 len1 ptr2 (fromIntegral len2)
    return (result /= 0)