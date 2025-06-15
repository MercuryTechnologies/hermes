{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.SimpleServer
  ( run
  , runWithPort
  , ServerSettings(..)
  , defaultSettings
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, handle, SomeException(..))
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString (send)
import Network.Wai
import Network.Wai.Internal
import Network.HTTP.Types (statusCode, statusMessage)
import System.IO (hClose, withFile, IOMode(..))
import Network.Wai.RequestParser (parseHttpRequest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString)
import Data.CaseInsensitive (CI(..), original)

data ServerSettings = ServerSettings
  { port :: Int
  , host :: String
  , backlog :: Int
  }

defaultSettings :: ServerSettings
defaultSettings = ServerSettings
  { port = 3000
  , host = "127.0.0.1"
  , backlog = 2048
  }

run :: Application -> IO ()
run = runWithPort defaultSettings

runWithPort :: ServerSettings -> Application -> IO ()
runWithPort settings app = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just $ host settings) (Just $ show $ port settings)

  bracket (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock $ addrAddress addr
    listen sock $ backlog settings

    putStrLn $ "Server running on http://" ++ host settings ++ ":" ++ show (port settings)

    forever $ do
      (clientSock, clientAddr) <- accept sock
      void $ forkIO $ handle (\(SomeException e) -> print e) $ do
        handleRequest clientSock app
        close clientSock

handleRequest :: Socket -> Application -> IO ()
handleRequest sock app = do
  -- Read and parse request
  request <- parseHttpRequest sock

  -- Create response
  void $ app request $ \response -> do
    sendResponse sock response
    return ResponseReceived

sendResponse :: Socket -> Response -> IO ()
sendResponse sock response = do
  let status = responseStatus response
      headers = responseHeaders response

  -- Send status line
  sendAll sock $ pack $ "HTTP/1.1 " ++ show (statusCode status) ++ " " ++ unpack (statusMessage status) ++ "\r\n"

  -- Send headers
  mapM_ (\(name, value) -> do
    sendAll sock (original name)
    sendAll sock ": "
    sendAll sock value
    sendAll sock "\r\n"
    ) headers

  -- Send empty line to separate headers from body
  sendAll sock "\r\n"

  -- Send body based on response type
  case response of
    ResponseFile _ _ filePath _ -> do
      withFile filePath ReadMode $ \handle -> do
        content <- LBS.hGetContents handle
        sendAll sock $ toStrict content

    ResponseBuilder _ _ builder -> do
      let bs = toStrict $ toLazyByteString builder
      sendAll sock bs

    ResponseStream _ _ streamBody -> do
      let sendChunk builder = do
            let bs = toStrict $ toLazyByteString builder
            sendAll sock bs
          flush = return ()  -- In a real implementation, we might want to ensure data is sent
      streamBody sendChunk flush

    ResponseRaw _ _ -> do
      -- For now, we'll just send an empty body for raw responses
      -- In a real implementation, we would need to handle the raw response properly
      return ()

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs = do
  sent <- send sock bs
  if sent < fromIntegral (BS.length bs)
    then sendAll sock (BS.drop sent bs)
    else return ()
