{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SimpleH2OServer
-- Description : Simple example demonstrating H2O HTTP server bindings
-- Copyright   : (c) 2024 Mercury Technologies
-- License     : BSD-3-Clause
-- Maintainer  : ian@mercury.com
-- Stability   : experimental
-- Portability : POSIX

module Main (main) where

import Network.H2O
import Data.ByteString.Char8 as C8
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  port <- case args of
    []      -> return 8080
    [portStr] -> case readMaybe portStr of
      Just p -> return p
      Nothing -> do
        putStrLn "Invalid port number"
        exitFailure
    _ -> do
      putStrLn "Usage: simple-h2o-server [port]"
      exitFailure

  putStrLn $ "Starting H2O server on port " ++ show port
  putStrLn "Press Ctrl+C to stop"

  result <- runH2OServer defaultConfig $ \server -> do
    -- Register various handlers
    registerHandler server "/" handleRoot
    registerHandler server "/hello" handleHello
    registerHandler server "/json" handleJson
    registerHandler server "/echo" handleEcho
    registerHandler server "/info" handleInfo

    putStrLn "Handlers registered:"
    putStrLn "  GET  /       - Root page"
    putStrLn "  GET  /hello  - Hello world"
    putStrLn "  GET  /json   - JSON response"
    putStrLn "  ANY  /echo   - Echo request details"
    putStrLn "  GET  /info   - Server information"

    -- Start the server
    bindAndListen server "127.0.0.1" port

  case result of
    Left err -> do
      putStrLn $ "Server error: " ++ err
      exitFailure
    Right () -> putStrLn "Server stopped"

-- | Handle root path
handleRoot :: Handler
handleRoot req = do
  let method = requestMethod req
  if method == "GET"
    then return $ responseHtml rootHtml
    else return $ Response 405 [] "Method Not Allowed"

-- | Handle hello path
handleHello :: Handler
handleHello req = do
  let method = requestMethod req
  if method == "GET"
    then return $ responseOk "Hello, World! This is H2O speaking."
    else return $ Response 405 [] "Method Not Allowed"

-- | Handle JSON response
handleJson :: Handler
handleJson req = do
  let method = requestMethod req
  if method == "GET"
    then return $ responseJson jsonResponse
    else return $ Response 405 [] "Method Not Allowed"
  where
    jsonResponse = C8.pack $ unlines
      [ "{"
      , "  \"message\": \"Hello from H2O!\","
      , "  \"server\": \"H2O Haskell Bindings\","
      , "  \"version\": \"0.1.0\","
      , "  \"features\": [\"HTTP/1.1\", \"HTTP/2\", \"WebSocket\"]"
      , "}"
      ]

-- | Echo request details
handleEcho :: Handler
handleEcho req = do
  let echoHtml = C8.pack $ unlines
        [ "<html><head><title>Echo</title></head><body>"
        , "<h1>Request Echo</h1>"
        , "<p><strong>Method:</strong> " ++ C8.unpack (requestMethod req) ++ "</p>"
        , "<p><strong>Path:</strong> " ++ C8.unpack (requestPath req) ++ "</p>"
        , "<h2>Headers:</h2>"
        , "<ul>"
        ] ++ map formatHeader (requestHeaders req) ++
        [ "</ul>"
        , "<h2>Body:</h2>"
        , "<pre>" ++ C8.unpack (requestBody req) ++ "</pre>"
        , "</body></html>"
        ]
  return $ responseHtml echoHtml
  where
    formatHeader (name, value) = 
      "<li><strong>" ++ C8.unpack name ++ ":</strong> " ++ C8.unpack value ++ "</li>"

-- | Handle server info
handleInfo :: Handler
handleInfo req = do
  let method = requestMethod req
  if method == "GET"
    then return $ responseHtml infoHtml
    else return $ Response 405 [] "Method Not Allowed"

-- HTML content

rootHtml :: ByteString
rootHtml = C8.pack $ unlines
  [ "<!DOCTYPE html>"
  , "<html><head><title>H2O Server</title></head><body>"
  , "<h1>Welcome to H2O Server</h1>"
  , "<p>This is a demonstration of the H2O Haskell bindings.</p>"
  , "<h2>Available Endpoints:</h2>"
  , "<ul>"
  , "  <li><a href=\"/hello\">/hello</a> - Simple hello world</li>"
  , "  <li><a href=\"/json\">/json</a> - JSON response</li>"
  , "  <li><a href=\"/echo\">/echo</a> - Echo request details</li>"
  , "  <li><a href=\"/info\">/info</a> - Server information</li>"
  , "</ul>"
  , "<h2>About H2O</h2>"
  , "<p>H2O is a high-performance HTTP server with support for:</p>"
  , "<ul>"
  , "  <li>HTTP/1.1 and HTTP/2</li>"
  , "  <li>WebSocket</li>"
  , "  <li>Server Push (HTTP/2)</li>"
  , "  <li>High performance and low latency</li>"
  , "</ul>"
  , "</body></html>"
  ]

infoHtml :: ByteString
infoHtml = C8.pack $ unlines
  [ "<!DOCTYPE html>"
  , "<html><head><title>Server Info</title></head><body>"
  , "<h1>Server Information</h1>"
  , "<table border=\"1\">"
  , "  <tr><td><strong>Server</strong></td><td>H2O via Haskell Bindings</td></tr>"
  , "  <tr><td><strong>Version</strong></td><td>0.1.0 (Experimental)</td></tr>"
  , "  <tr><td><strong>Protocol Support</strong></td><td>HTTP/1.1, HTTP/2</td></tr>"
  , "  <tr><td><strong>Language</strong></td><td>Haskell</td></tr>"
  , "  <tr><td><strong>Backend</strong></td><td>libh2o</td></tr>"
  , "</table>"
  , "<h2>Features</h2>"
  , "<ul>"
  , "  <li>Type-safe HTTP server implementation</li>"
  , "  <li>Zero-copy I/O where possible</li>"
  , "  <li>Automatic resource management</li>"
  , "  <li>Exception safety</li>"
  , "  <li>Property-based testing</li>"
  , "</ul>"
  , "<p><a href=\"/\">Back to home</a></p>"
  , "</body></html>"
  ]