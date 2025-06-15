{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.RequestParser
  ( parseHttpRequest
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (find)
import Network.Socket (Socket(..), SockAddr(..))
import Network.Socket.ByteString (recv)
import Network.Wai
import Network.HTTP.Types (HttpVersion(..), Header, HeaderName)
import Data.CaseInsensitive (CI(..), mk)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

parseHttpRequest :: Socket -> IO Request
parseHttpRequest sock = do
  -- Read initial buffer
  initialBuf <- recv sock 4096
  let (requestLine, headersBuf) = BS.breakSubstring "\r\n" initialBuf
  putStrLn $ "Debug - Raw request line: " ++ show (unpack requestLine)

  -- Parse request line
  let requestParts = words $ unpack requestLine
  putStrLn $ "Debug - Request parts: " ++ show requestParts
  (method, path, version) <- case requestParts of
    (m:p:v:_) -> return (m, p, v)
    _ -> fail "Invalid request line format - expected METHOD PATH VERSION"
  putStrLn $ "Debug - Parsed method: " ++ method
  putStrLn $ "Debug - Parsed path: " ++ path
  putStrLn $ "Debug - Parsed version: " ++ version

  -- Parse headers
  let (headers, bodyBuf) = parseHeaders headersBuf
  putStrLn $ "Debug - Headers: " ++ show headers

  -- Parse content length
  let contentLength = maybe 0 (read . unpack) $
        find ((== "Content-Length") . fst) headers >>= Just . snd
  putStrLn $ "Debug - Content length: " ++ show contentLength

  -- Read remaining body if needed
  body <- if contentLength > 0
    then do
      let remainingLength = contentLength - BS.length bodyBuf
      if remainingLength <= 0
        then do
          putStrLn $ "Debug - Body fully contained in initial buffer"
          return bodyBuf
        else do
          putStrLn $ "Debug - Reading additional body data of length " ++ show remainingLength
          additionalBody <- recv sock remainingLength
          return $ bodyBuf `BS.append` additionalBody
    else do
      putStrLn "Debug - No body to read"
      return ""

  -- Parse query string
  let (path', query) = break (== '?') path
  putStrLn $ "Debug - Path without query: " ++ path'
  putStrLn $ "Debug - Query string: " ++ query

  let parsedRequest = defaultRequest
        { requestMethod = pack method
        , httpVersion = parseVersion version
        , rawPathInfo = pack path'
        , rawQueryString = pack $ drop 1 query
        , requestHeaders = map convertHeader headers
        , isSecure = False
        , remoteHost = SockAddrInet 0 0
        , pathInfo = map decodeUtf8 $ parsePath path'
        , queryString = parseQuery query
        , requestBody = return body
        , vault = mempty
        , requestBodyLength = if contentLength > 0
            then KnownLength $ fromIntegral contentLength
            else ChunkedBody
        , requestHeaderHost = find ((== "Host") . fst) headers >>= Just . snd
        , requestHeaderRange = find ((== "Range") . fst) headers >>= Just . snd
        }
  putStrLn $ "Debug - Final parsed request: " ++ show parsedRequest
  return parsedRequest

convertHeader :: (ByteString, ByteString) -> Header
convertHeader (name, value) = (mk name, value)

readLine :: Socket -> IO ByteString
readLine sock = do
  line <- recv sock 1024
  if BS.null line
    then return BS.empty
    else do
      let (line', rest) = BS.breakSubstring "\r\n" line
      if BS.null rest
        then do
          -- Line continues in next recv
          next <- readLine sock
          return $ line `BS.append` next
        else do
          -- Found complete line
          return line'

readHeaders :: Socket -> IO [(ByteString, ByteString)]
readHeaders sock = do
  line <- readLine sock
  if BS.null line
    then return []
    else do
      let (name, value) = BS.breakSubstring ": " line
      rest <- readHeaders sock
      return $ (name, BS.drop 2 value) : rest

parseVersion :: String -> HttpVersion
parseVersion "HTTP/1.0" = HttpVersion 1 0
parseVersion "HTTP/1.1" = HttpVersion 1 1
parseVersion _ = HttpVersion 1 1

parsePath :: String -> [ByteString]
parsePath = filter (not . BS.null) . map pack . split '/'

parseQuery :: String -> [(ByteString, Maybe ByteString)]
parseQuery = map parseQueryParam . split '&' . drop 1

parseQueryParam :: String -> (ByteString, Maybe ByteString)
parseQueryParam param = case break (== '=') param of
  (key, "") -> (pack key, Nothing)
  (key, val) -> (pack key, Just $ pack $ drop 1 val)

split :: Char -> String -> [String]
split c s = case break (== c) s of
  (a, "") -> [a]
  (a, b) -> a : split c (drop 1 b)

parseHeaders :: ByteString -> ([(ByteString, ByteString)], ByteString)
parseHeaders buf = go buf []
  where
    go :: ByteString -> [(ByteString, ByteString)] -> ([(ByteString, ByteString)], ByteString)
    go b acc = case BS.breakSubstring "\r\n" b of
      (line, rest) | BS.null line -> (reverse acc, BS.drop 2 rest)  -- Empty line marks end of headers
      (line, rest) -> case BS.breakSubstring ": " line of
        (name, value) -> go (BS.drop 2 rest) ((name, BS.drop 2 value) : acc)
        _ -> (reverse acc, b)  -- Invalid header format, treat as body
