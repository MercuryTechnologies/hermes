{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.SimpleServer
import Network.HTTP.Types (status200, status404)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = run app

app :: Application
app request respond = do
  case (requestMethod request, pathInfo request) of
    ("GET", []) -> respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
    ("GET", ["hello"]) -> respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello from /hello!"
    ("GET", ["echo"]) -> do
      body <- requestBody request
      respond $ responseLBS status200 [("Content-Type", "text/plain")] (BL.fromStrict body)
    _ -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"
