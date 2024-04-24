{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Headers.CacheStatusSpec (spec) where

import qualified Data.Text.Short as TS
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Mason.Builder as M
import Network.HTTP.Headers.CacheStatus
import qualified Network.HTTP.Headers.Parsing.Util as P
import qualified Network.HTTP.Headers.Rendering.Util as R
import RFC8941
import Test.Hspec
import Test.Hspec.Hedgehog

cacheStatusGen :: Gen CacheStatus
cacheStatusGen = CacheStatus <$> do
  Gen.list (Range.linear 0 40) cacheStatusEntryGen

cacheStatusEntryGen :: Gen CacheStatusEntry
cacheStatusEntryGen = do
  cacheStatusIdentifier <- Gen.either tokenGen stringGen
  cacheStatusHitOrForward <- Gen.either
    (pure Hit)
    ( do
      forwardType <- Gen.choice $ fmap pure
        [ Bypass
        , Method
        , UriMiss
        , VaryMiss
        , Miss
        , Request
        , Stale
        , Partial
        ]
      forwardStatus <- Gen.maybe (Gen.integral (Range.linear 100 599))
      forwardStored <- Gen.maybe Gen.bool
      forwardCollapsed <- Gen.maybe Gen.bool
      pure ForwardInfo{..}
    )

  cacheStatusTtl <- Gen.maybe (Gen.integral (Range.linear (-1_000_000) 1_000_000))
  cacheStatusKey <- Gen.maybe stringGen
  cacheStatusDetail <- Gen.maybe (Gen.either tokenGen stringGen)
  let cacheStatusUnknownParameters = []
      cacheStatusCommonFields = CacheStatusCommonFields{..}
  pure CacheStatusEntry{..}

spec :: Spec
spec = do
  it "should be able to deserialize valid cache status info" $ hedgehog $ do
    t <- forAll cacheStatusGen
    tripping t (M.toStrictByteString . renderCacheStatus) $ \bs -> case P.runParser cacheStatusParser bs of
      P.OK t' "" -> Right t'
      P.OK _ rest -> Left ("Unconsumed input after parsing CacheStatus header: " <> show rest)
      P.Fail -> Left "Failed to parse CacheStatus header"
      P.Err err -> Left $ show err
