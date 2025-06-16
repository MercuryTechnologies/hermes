{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.H2O
-- Description : High-level Haskell bindings for H2O HTTP server
-- Copyright   : (c) 2024 Mercury Technologies
-- License     : BSD-3-Clause
-- Maintainer  : ian@mercury.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a high-level, type-safe interface to the H2O HTTP
-- server library. It includes automatic resource management, exception
-- handling, and Haskell-friendly APIs.
--
-- = Example Usage
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Network.H2O
-- import Data.ByteString.Char8 as C8
-- 
-- main :: IO ()
-- main = do
--   result <- runH2OServer defaultConfig $ \server -> do
--     registerHandler server "/" $ \req -> do
--       let method = requestMethod req
--       if method == "GET"
--         then return $ Response 200 [("Content-Type", "text/plain")] "Hello, World!"
--         else return $ Response 405 [] ""
--     bindAndListen server "127.0.0.1" 8080
--   case result of
--     Left err -> putStrLn $ "Server error: " ++ err
--     Right () -> putStrLn "Server finished"
-- @

module Network.H2O
  ( -- * Server Management
    H2OServer
  , H2OConfig(..)
  , defaultConfig
  , runH2OServer
  , bindAndListen
  , stopServer

    -- * Request Handling
  , Request(..)
  , Response(..)
  , Handler
  , registerHandler
  , requestMethod
  , requestPath
  , requestHeaders
  , requestBody

    -- * Response Building
  , responseOk
  , responseNotFound
  , responseError
  , responseJson
  , responseHtml

    -- * Error Handling
  , H2OException(..)

    -- * Re-exports from lower-level modules
  , module Network.H2O.Types
  ) where

import Network.H2O.Types
import Network.H2O.Core
import Control.Exception (Exception, bracket, throwIO, catch)
import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, newMVar, modifyMVar_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as NS
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable)

-- | Configuration for H2O server
data H2OConfig = H2OConfig
  { h2oConfigMaxRequestSize :: !Int     -- ^ Maximum request size in bytes
  , h2oConfigTimeout        :: !Int     -- ^ Request timeout in seconds
  , h2oConfigKeepAlive      :: !Bool    -- ^ Enable keep-alive connections
  , h2oConfigH2            :: !Bool    -- ^ Enable HTTP/2 support
  } deriving (Show, Eq)

-- | Default H2O configuration
defaultConfig :: H2OConfig
defaultConfig = H2OConfig
  { h2oConfigMaxRequestSize = 1024 * 1024  -- 1MB
  , h2oConfigTimeout = 30
  , h2oConfigKeepAlive = True
  , h2oConfigH2 = True
  }

-- | HTTP Request representation
data Request = Request
  { requestMethod  :: !ByteString
  , requestPath    :: !ByteString
  , requestHeaders :: ![(ByteString, ByteString)]
  , requestBody    :: !ByteString
  } deriving (Show, Eq)

-- | HTTP Response representation
data Response = Response
  { responseStatus  :: !Int
  , responseHeaders :: ![(ByteString, ByteString)]
  , responseBody    :: !ByteString
  } deriving (Show, Eq)

-- | Request handler type
type Handler = Request -> IO Response

-- | H2O server handle
data H2OServer = H2OServer
  { h2oServerConfig     :: !H2OConfig
  , h2oServerGlobalConf :: !(Ptr H2OGlobalConf)
  , h2oServerContext    :: !(Ptr H2OContext)
  , h2oServerEvLoop     :: !(Ptr H2OEvLoop)
  , h2oServerHostConf   :: !(Ptr H2OHostConf)
  , h2oServerHandlers   :: !(IORef [(ByteString, Handler)])
  , h2oServerRunning    :: !(MVar ())
  }

-- | H2O specific exceptions
data H2OException
  = H2OInitializationError String
  | H2OBindError String
  | H2OHandlerError String
  | H2OMemoryError String
  deriving (Show, Eq, Typeable)

instance Exception H2OException

-- | Run H2O server with automatic resource management
runH2OServer :: H2OConfig -> (H2OServer -> IO a) -> IO (Either String a)
runH2OServer config action = do
  result <- catch (Right <$> bracket createServer destroyServer action) handleException
  return result
  where
    handleException :: H2OException -> IO (Either String a)
    handleException ex = return $ Left $ show ex

    createServer = do
      -- Allocate server structures
      globalConf <- mallocBytes (#size h2o_globalconf_t)
      context <- mallocBytes (#size h2o_context_t)
      
      -- Initialize global configuration
      h2oConfigInit globalConf
      
      -- Create event loop
      evLoop <- h2oEvloopCreate
      if evLoop == nullPtr
        then throwIO $ H2OInitializationError "Failed to create event loop"
        else return ()
      
      -- Initialize context
      h2oContextInit context evLoop globalConf
      
      -- Register default host
      hostConf <- withCString "default" $ \hostname -> do
        withH2OString "default" $ \hostStr ->
          h2oConfigRegisterHost globalConf hostStr 65535
      
      -- Create handler registry
      handlers <- newIORef []
      running <- newEmptyMVar
      
      return $ H2OServer config globalConf context evLoop hostConf handlers running

    destroyServer server = do
      h2oContextDispose (h2oServerContext server)
      h2oEvloopDestroy (h2oServerEvLoop server)
      h2oConfigDispose (h2oServerGlobalConf server)
      free (h2oServerGlobalConf server)
      free (h2oServerContext server)

-- | Register a request handler for a specific path
registerHandler :: H2OServer -> ByteString -> Handler -> IO ()
registerHandler server path handler = do
  -- Add to handler registry
  modifyIORef' (h2oServerHandlers server) ((path, handler):)
  
  -- Register with H2O
  C8.useAsCString path $ \pathPtr -> do
    pathConf <- h2oConfigRegisterPath (h2oServerHostConf server) pathPtr 0
    _ <- h2oRegisterRequestHandler pathConf (h2oHandlerWrapper handler)
    return ()

-- | Internal C callback wrapper for Haskell handlers
h2oHandlerWrapper :: Handler -> H2OHandlerCallback
h2oHandlerWrapper handler handlerPtr reqPtr = do
  result <- catch (handleRequest handler reqPtr) handleError
  case result of
    Right () -> return 0
    Left _   -> return (-1)
  where
    handleError :: H2OException -> IO (Either H2OException ())
    handleError ex = return $ Left ex

-- | Convert H2O request to Haskell Request and call handler
handleRequest :: Handler -> Ptr H2ORequest -> IO (Either H2OException ())
handleRequest handler reqPtr = do
  -- Extract request data
  methodRef <- (#peek h2o_req_t, method) reqPtr
  pathRef <- (#peek h2o_req_t, path) reqPtr
  
  method <- h2oStringToByteString methodRef
  path <- h2oStringToByteString pathRef
  
  -- For simplicity, we'll start with empty headers and body
  let request = Request method path [] ""
  
  -- Call handler
  response <- handler request
  
  -- Send response
  sendResponse reqPtr response
  
  return $ Right ()

-- | Send HTTP response through H2O
sendResponse :: Ptr H2ORequest -> Response -> IO ()
sendResponse reqPtr (Response status headers body) = do
  -- Set status
  (#poke h2o_req_t, res.status) reqPtr (fromIntegral status :: CInt)
  (#poke h2o_req_t, res.reason) reqPtr =<< newCString (statusReason status)
  
  -- Add headers
  pool <- (#peek h2o_req_t, pool) reqPtr
  headersPtr <- (#peek h2o_req_t, res.headers) reqPtr
  
  mapM_ (addResponseHeader pool headersPtr) headers
  
  -- Send body
  h2oSendByteStringInline reqPtr body

-- | Add a response header
addResponseHeader :: Ptr H2OMemPool -> Ptr (H2OVector H2OIovec) -> (ByteString, ByteString) -> IO ()
addResponseHeader pool headersPtr (name, value) = do
  C8.useAsCString name $ \namePtr ->
    C8.useAsCString value $ \valuePtr ->
      h2oAddHeader pool headersPtr namePtr nullPtr valuePtr (fromIntegral $ C8.length value)

-- | Get status reason phrase
statusReason :: Int -> String
statusReason 200 = "OK"
statusReason 404 = "Not Found"
statusReason 500 = "Internal Server Error"
statusReason 400 = "Bad Request"
statusReason 405 = "Method Not Allowed"
statusReason _   = "Unknown"

-- | Bind server to address and start listening
bindAndListen :: H2OServer -> String -> Int -> IO ()
bindAndListen server host port = do
  -- Create and bind socket
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  addr <- NS.getAddrInfo Nothing (Just host) (Just $ show port) >>= \case
    (ai:_) -> return $ NS.addrAddress ai
    []     -> throwIO $ H2OBindError $ "Cannot resolve address: " ++ host
  
  NS.bind sock addr
  NS.listen sock 128
  
  -- Convert to file descriptor and attach to H2O
  fd <- NS.fdSocket sock
  h2oSock <- h2oEvloopSocketCreate (h2oServerEvLoop server) (fromIntegral fd) H2O_SOCKET_FLAG_DONT_READ
  
  acceptCallback <- wrapH2OSocketCallback (onAccept server)
  h2oSocketReadStart h2oSock acceptCallback
  
  -- Start event loop
  putMVar (h2oServerRunning server) ()
  _ <- h2oEvloopRun (h2oServerEvLoop server) maxBound
  return ()

-- | Accept callback for new connections
onAccept :: H2OServer -> H2OSocketCallback
onAccept server listener err = do
  if err == nullPtr
    then do
      -- Accept new connection
      newSock <- h2oEvloopSocketAccept listener
      if newSock /= nullPtr
        then do
          -- Create accept context and accept connection
          acceptCtx <- mallocBytes (#size h2o_accept_ctx_t)
          (#poke h2o_accept_ctx_t, ctx) acceptCtx (h2oServerContext server)
          (#poke h2o_accept_ctx_t, hosts) acceptCtx =<< (#peek h2o_globalconf_t, hosts) (h2oServerGlobalConf server)
          h2oAccept acceptCtx newSock
          free acceptCtx
        else return ()
    else return ()

foreign import ccall unsafe "h2o_evloop_socket_accept"
  h2oEvloopSocketAccept :: Ptr H2OSocket -> IO (Ptr H2OSocket)

foreign import ccall "wrapper"
  wrapH2OSocketCallback :: H2OSocketCallback -> IO (FunPtr H2OSocketCallback)

-- | Stop the server
stopServer :: H2OServer -> IO ()
stopServer _server = do
  -- Implementation would signal the event loop to stop
  -- For now, this is a placeholder
  return ()

-- Response builders

-- | Create a 200 OK response
responseOk :: ByteString -> Response
responseOk body = Response 200 [("Content-Type", "text/plain")] body

-- | Create a 404 Not Found response  
responseNotFound :: Response
responseNotFound = Response 404 [("Content-Type", "text/plain")] "Not Found"

-- | Create a 500 Internal Server Error response
responseError :: ByteString -> Response
responseError body = Response 500 [("Content-Type", "text/plain")] body

-- | Create a JSON response
responseJson :: ByteString -> Response  
responseJson body = Response 200 [("Content-Type", "application/json")] body

-- | Create an HTML response
responseHtml :: ByteString -> Response
responseHtml body = Response 200 [("Content-Type", "text/html")] body