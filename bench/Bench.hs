{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.Accept
import Network.HTTP.Headers.AcceptEncoding
import Network.HTTP.Headers.Age
import Network.HTTP.Headers.Authorization
import Network.HTTP.Headers.CacheControl
import Network.HTTP.Headers.ContentEncoding
import Network.HTTP.Headers.ContentLength
import Network.HTTP.Headers.ContentType
import Network.HTTP.Headers.Expires
import Network.HTTP.Headers.IfModifiedSince
import Network.HTTP.Headers.IfUnmodifiedSince
import Network.HTTP.Headers.LastModified
import Network.HTTP.Headers.Location
import Network.HTTP.Headers.PingFrom
import Network.HTTP.Headers.ProxyAuthorization
import Network.HTTP.Headers.Referer
import Network.HTTP.Headers.Sunset
import Network.HTTP.Headers.TransferEncoding
import Network.HTTP.ContentCoding (ContentCoding(..))
import Network.HTTP.ContentNegotiation
import Network.HTTP.Headers.Authorization (Credentials(..))
import Data.Word (Word32)

-- Sample data for benchmarks
sampleTime :: IO UTCTime
sampleTime = getCurrentTime

-- Accept headers with different numbers of media types
sampleAcceptShort :: IO Accept
sampleAcceptShort = pure $ Accept [WeightedMediaRange (MediaRange (MediaType "text" "html") []) 1.0]

sampleAcceptMedium :: IO Accept
sampleAcceptMedium = pure $ Accept
  [ WeightedMediaRange (MediaRange (MediaType "text" "html") []) 1.0
  , WeightedMediaRange (MediaRange (MediaType "application" "json") []) 0.8
  , WeightedMediaRange (MediaRange (MediaType "text" "plain") []) 0.5
  ]

sampleAcceptLong :: IO Accept
sampleAcceptLong = pure $ Accept
  [ WeightedMediaRange (MediaRange (MediaType "text" "html") []) 1.0
  , WeightedMediaRange (MediaRange (MediaType "application" "json") []) 0.8
  , WeightedMediaRange (MediaRange (MediaType "text" "plain") []) 0.5
  , WeightedMediaRange (MediaRange (MediaType "image" "png") []) 0.3
  , WeightedMediaRange (MediaRange (MediaType "image" "jpeg") []) 0.3
  , WeightedMediaRange (MediaRange (MediaType "application" "xml") []) 0.2
  ]

-- Accept-Encoding headers with different numbers of encodings
sampleAcceptEncodingShort :: IO AcceptEncoding
sampleAcceptEncodingShort = pure $ AcceptEncoding GZip

sampleAcceptEncodingMedium :: IO AcceptEncoding
sampleAcceptEncodingMedium = pure $ AcceptEncoding (Custom "gzip,deflate")

sampleAcceptEncodingLong :: IO AcceptEncoding
sampleAcceptEncodingLong = pure $ AcceptEncoding (Custom "gzip,deflate,compress,identity")

-- Age headers with different values
sampleAgeShort :: IO Age
sampleAgeShort = pure $ Age 60

sampleAgeMedium :: IO Age
sampleAgeMedium = pure $ Age 3600

sampleAgeLong :: IO Age
sampleAgeLong = pure $ Age 86400

-- Authorization headers with different credentials
sampleAuthorizationShort :: IO Authorization
sampleAuthorizationShort = pure $ Authorization
  Credentials
    { scheme = AuthScheme "Basic"
    , contents = CredentialToken "user:pass"
    }

sampleAuthorizationMedium :: IO Authorization
sampleAuthorizationMedium = pure $ Authorization
  Credentials
    { scheme = AuthScheme "Bearer"
    , contents = CredentialToken "medium.length.token"
    }

sampleAuthorizationLong :: IO Authorization
sampleAuthorizationLong = pure $ Authorization
  Credentials
    { scheme = AuthScheme "Bearer"
    , contents = CredentialToken "very.long.authentication.token.with.many.characters"
    }

-- Cache-Control headers with different numbers of directives
sampleCacheControlShort :: IO CacheControl
sampleCacheControlShort = pure $ CacheControl $ NE.fromList [MaxAge 3600]

sampleCacheControlMedium :: IO CacheControl
sampleCacheControlMedium = pure $ CacheControl $ NE.fromList
  [ MaxAge 3600
  , NoCache Nothing
  , MustRevalidate
  ]

sampleCacheControlLong :: IO CacheControl
sampleCacheControlLong = pure $ CacheControl $ NE.fromList
  [ MaxAge 3600
  , NoCache Nothing
  , MustRevalidate
  , Private Nothing
  , NoStore
  , ProxyRevalidate
  , Immutable
  ]

-- Content-Encoding headers with different encodings
sampleContentEncodingShort :: IO ContentEncoding
sampleContentEncodingShort = pure $ ContentEncoding GZip

sampleContentEncodingMedium :: IO ContentEncoding
sampleContentEncodingMedium = pure $ ContentEncoding (Custom "gzip,deflate")

sampleContentEncodingLong :: IO ContentEncoding
sampleContentEncodingLong = pure $ ContentEncoding (Custom "gzip,deflate,compress,identity")

-- Content-Length headers with different values
sampleContentLengthShort :: IO ContentLength
sampleContentLengthShort = pure $ ContentLength 1024

sampleContentLengthMedium :: IO ContentLength
sampleContentLengthMedium = pure $ ContentLength 1048576

sampleContentLengthLong :: IO ContentLength
sampleContentLengthLong = pure $ ContentLength 1073741824

-- Content-Type headers with different media types
sampleContentTypeShort :: IO ContentType
sampleContentTypeShort = pure $ ContentType (MediaType "text" "plain")

sampleContentTypeMedium :: IO ContentType
sampleContentTypeMedium = pure $ ContentType (MediaType "application" "json")

sampleContentTypeLong :: IO ContentType
sampleContentTypeLong = pure $ ContentType (MediaType "multipart" "form-data")

-- Time-based headers with different times
sampleTimeShort :: IO UTCTime
sampleTimeShort = getCurrentTime

sampleTimeMedium :: IO UTCTime
sampleTimeMedium = addUTCTime 3600 <$> getCurrentTime

sampleTimeLong :: IO UTCTime
sampleTimeLong = addUTCTime 86400 <$> getCurrentTime

-- Location headers with different URL lengths
sampleLocationShort :: IO Location
sampleLocationShort = pure $ Location "https://example.com"

sampleLocationMedium :: IO Location
sampleLocationMedium = pure $ Location "https://example.com/path/to/resource"

sampleLocationLong :: IO Location
sampleLocationLong = pure $ Location "https://example.com/path/to/resource/with/many/segments/and/query?param1=value1&param2=value2"

-- Ping-From headers with different lengths
samplePingFromShort :: IO PingFrom
samplePingFromShort = pure $ PingFrom "example.com"

samplePingFromMedium :: IO PingFrom
samplePingFromMedium = pure $ PingFrom "subdomain.example.com"

samplePingFromLong :: IO PingFrom
samplePingFromLong = pure $ PingFrom "very.long.subdomain.example.com"

-- Proxy-Authorization headers with different credentials
sampleProxyAuthorizationShort :: IO ProxyAuthorization
sampleProxyAuthorizationShort = pure $ ProxyAuthorization
  Credentials
    { scheme = AuthScheme "Basic"
    , contents = CredentialToken "user:pass"
    }

sampleProxyAuthorizationMedium :: IO ProxyAuthorization
sampleProxyAuthorizationMedium = pure $ ProxyAuthorization
  Credentials
    { scheme = AuthScheme "Bearer"
    , contents = CredentialToken "medium.length.token"
    }

sampleProxyAuthorizationLong :: IO ProxyAuthorization
sampleProxyAuthorizationLong = pure $ ProxyAuthorization
  Credentials
    { scheme = AuthScheme "Bearer"
    , contents = CredentialToken "very.long.authentication.token.with.many.characters"
    }

-- Referer headers with different URL lengths
sampleRefererShort :: IO Referer
sampleRefererShort = pure $ Referer "https://example.com"

sampleRefererMedium :: IO Referer
sampleRefererMedium = pure $ Referer "https://example.com/path/to/resource"

sampleRefererLong :: IO Referer
sampleRefererLong = pure $ Referer "https://example.com/path/to/resource/with/many/segments/and/query?param1=value1&param2=value2"

-- Transfer-Encoding headers with different numbers of encodings
sampleTransferEncodingShort :: IO TransferEncoding
sampleTransferEncodingShort = pure $ TransferEncoding $ NE.fromList ["chunked"]

sampleTransferEncodingMedium :: IO TransferEncoding
sampleTransferEncodingMedium = pure $ TransferEncoding $ NE.fromList ["chunked", "gzip"]

sampleTransferEncodingLong :: IO TransferEncoding
sampleTransferEncodingLong = pure $ TransferEncoding $ NE.fromList ["chunked", "gzip", "deflate", "compress"]

-- Helper function to create a NonEmpty ByteString for parsing
mkHeader :: BS.ByteString -> NE.NonEmpty BS.ByteString
mkHeader = NE.singleton

-- Helper function to create benchmarks for a header type
benchHeader :: forall a. (KnownHeader a, Show a, HeaderRenderingResultToNonEmpty (Cardinality a)) => String -> IO a -> Benchmark
benchHeader name valueIO = bgroup name
  [ bench "encode" $ whnfIO $ do
      value <- valueIO
      pure $ renderToHeaders defaultHeaderSettings value
  , bench "decode" $ whnfIO $ do
      value <- valueIO
      let rendered = renderToHeaders defaultHeaderSettings value
      pure $ parseFromHeaders @a defaultHeaderSettings $ headerRenderingResultToNonEmpty @(Cardinality a) rendered
  ]

main :: IO ()
main = do
  expiresShort <- Expires <$> sampleTimeShort
  expiresMedium <- Expires <$> sampleTimeMedium
  expiresLong <- Expires <$> sampleTimeLong

  ifModifiedSinceShort <- IfModifiedSince <$> sampleTimeShort
  ifModifiedSinceMedium <- IfModifiedSince <$> sampleTimeMedium
  ifModifiedSinceLong <- IfModifiedSince <$> sampleTimeLong

  ifUnmodifiedSinceShort <- IfUnmodifiedSince <$> sampleTimeShort
  ifUnmodifiedSinceMedium <- IfUnmodifiedSince <$> sampleTimeMedium
  ifUnmodifiedSinceLong <- IfUnmodifiedSince <$> sampleTimeLong

  lastModifiedShort <- LastModified <$> sampleTimeShort
  lastModifiedMedium <- LastModified <$> sampleTimeMedium
  lastModifiedLong <- LastModified <$> sampleTimeLong

  sunsetShort <- Sunset <$> sampleTimeShort
  sunsetMedium <- Sunset <$> sampleTimeMedium
  sunsetLong <- Sunset <$> sampleTimeLong

  defaultMain
    [ bgroup "Accept"
      [ benchHeader "short" sampleAcceptShort
      , benchHeader "medium" sampleAcceptMedium
      , benchHeader "long" sampleAcceptLong
      ]
    , bgroup "Accept-Encoding"
      [ benchHeader "short" sampleAcceptEncodingShort
      , benchHeader "medium" sampleAcceptEncodingMedium
      , benchHeader "long" sampleAcceptEncodingLong
      ]
    , bgroup "Age"
      [ benchHeader "short" sampleAgeShort
      , benchHeader "medium" sampleAgeMedium
      , benchHeader "long" sampleAgeLong
      ]
    , bgroup "Authorization"
      [ benchHeader "short" sampleAuthorizationShort
      , benchHeader "medium" sampleAuthorizationMedium
      , benchHeader "long" sampleAuthorizationLong
      ]
    , bgroup "Cache-Control"
      [ benchHeader "short" sampleCacheControlShort
      , benchHeader "medium" sampleCacheControlMedium
      , benchHeader "long" sampleCacheControlLong
      ]
    , bgroup "Content-Encoding"
      [ benchHeader "short" sampleContentEncodingShort
      , benchHeader "medium" sampleContentEncodingMedium
      , benchHeader "long" sampleContentEncodingLong
      ]
    , bgroup "Content-Length"
      [ benchHeader "short" sampleContentLengthShort
      , benchHeader "medium" sampleContentLengthMedium
      , benchHeader "long" sampleContentLengthLong
      ]
    , bgroup "Content-Type"
      [ benchHeader "short" sampleContentTypeShort
      , benchHeader "medium" sampleContentTypeMedium
      , benchHeader "long" sampleContentTypeLong
      ]
    , bgroup "Expires"
      [ benchHeader "short" (pure expiresShort)
      , benchHeader "medium" (pure expiresMedium)
      , benchHeader "long" (pure expiresLong)
      ]
    , bgroup "If-Modified-Since"
      [ benchHeader "short" (pure ifModifiedSinceShort)
      , benchHeader "medium" (pure ifModifiedSinceMedium)
      , benchHeader "long" (pure ifModifiedSinceLong)
      ]
    , bgroup "If-Unmodified-Since"
      [ benchHeader "short" (pure ifUnmodifiedSinceShort)
      , benchHeader "medium" (pure ifUnmodifiedSinceMedium)
      , benchHeader "long" (pure ifUnmodifiedSinceLong)
      ]
    , bgroup "Last-Modified"
      [ benchHeader "short" (pure lastModifiedShort)
      , benchHeader "medium" (pure lastModifiedMedium)
      , benchHeader "long" (pure lastModifiedLong)
      ]
    , bgroup "Location"
      [ benchHeader "short" sampleLocationShort
      , benchHeader "medium" sampleLocationMedium
      , benchHeader "long" sampleLocationLong
      ]
    , bgroup "Ping-From"
      [ benchHeader "short" samplePingFromShort
      , benchHeader "medium" samplePingFromMedium
      , benchHeader "long" samplePingFromLong
      ]
    , bgroup "Proxy-Authorization"
      [ benchHeader "short" sampleProxyAuthorizationShort
      , benchHeader "medium" sampleProxyAuthorizationMedium
      , benchHeader "long" sampleProxyAuthorizationLong
      ]
    , bgroup "Referer"
      [ benchHeader "short" sampleRefererShort
      , benchHeader "medium" sampleRefererMedium
      , benchHeader "long" sampleRefererLong
      ]
    , bgroup "Sunset"
      [ benchHeader "short" (pure sunsetShort)
      , benchHeader "medium" (pure sunsetMedium)
      , benchHeader "long" (pure sunsetLong)
      ]
    , bgroup "Transfer-Encoding"
      [ benchHeader "short" sampleTransferEncodingShort
      , benchHeader "medium" sampleTransferEncodingMedium
      , benchHeader "long" sampleTransferEncodingLong
      ]
    ]

