module Network.HTTP.Headers.CacheControlSpec (spec) where
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.List
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Network.HTTP.Headers
import Network.HTTP.Headers.CacheControl
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

validHeaders :: [ByteString]
validHeaders = 
  [ "max-age=3600"
  , "no-cache"
  , "no-store"
  , "public, max-age=3600"
  , "private, max-age=86400"
  , "no-cache, no-store"
  , "must-revalidate"
  , "proxy-revalidate"
  , "max-age=0"
  , "s-maxage=3600"
  , "public, must-revalidate"
  , "private, must-revalidate"
  , "immutable"
  , "no-transform"
  , "only-if-cached"
  , "max-stale=60"
  , "min-fresh=30"
  , "stale-while-revalidate=86400"
  , "stale-if-error=3600"
  , "public, immutable, max-age=31536000"
  , "private, no-cache, no-store, must-revalidate"
  , "max-age=600, must-revalidate"
  , "s-maxage=600, proxy-revalidate"
  , "no-cache, max-age=0"
  , "no-store, max-age=0"
  , "must-revalidate, proxy-revalidate"
  , "max-age=3600, stale-while-revalidate=600"
  , "max-age=3600, stale-if-error=600"
  , "public, s-maxage=600, max-age=60"
  , "private, max-age=3600, no-transform"
  ]

incoherentHeaders :: [ByteString]
incoherentHeaders = 
  [ "public, private"
  , "no-cache, max-age=3600"
  , "only-if-cached, max-age=3600"
  , "no-store, s-maxage=3600"
  , "immutable, must-revalidate"
  , "public, no-store"
  , "s-maxage=60, max-age=30"
  , "proxy-revalidate, private"
  , "min-stale=30"
  , "fresh-until=2023-01-01"
  , "cache-control=no-cache"
  , "revalidate"
  , "maxage=3600"
  , "public, secret"
  , "store"
  , "max-age=3600, no-cache, public"
  , "must-revalidate, immutable"
  , "proxy-revalidate, must-not-revalidate"
  , "s-maximum-age=600"
  , "max-age=3600, stale-while-validate=600"
  , "private, public"
  , "cacheonly"
  , "nostore"
  , "max-age=3600, min-fresh"
  , "public, only-if-not-cached"
  ]

invalidHeaders :: [ByteString]
invalidHeaders = 
  [ "max-age=one-hour"
  , "max-age=-1"
  , "stale-if-error"
  , "stale-while-revalidate"
  , "no-transform, max-age=text"
  ]

spec :: Spec
spec = do
  describe "parseCacheControlHeader" $ do
    context "when given a valid Cache-Control header" $ do
      for_ validHeaders $ \header -> do
        it ("parses valid header: " ++ C.unpack header) $ do
          parseCacheControlHeader header `shouldSatisfy` isRight

    context "when given an incoherent Cache-Control header" $ do
      for_ incoherentHeaders $ \header -> do
        it ("successfully parses incoherent header: " ++ C.unpack header) $ do
          (parseCacheControlHeader header `shouldSatisfy` isRight) :: IO ()
        -- it ("fails validation of incoherent header: " ++ C.unpack header) $ do
        --   fail "not implemented"

    context "when given an incoherent Cache-Control header" $ do
      for_ invalidHeaders $ \header -> do
        it ("fails to parse invalid header: " ++ C.unpack header) $ do
          case parseCacheControlHeader header of
            Left _ -> pure ()
            Right xs -> expectationFailure ("expected parsing to fail: " ++ show xs)

