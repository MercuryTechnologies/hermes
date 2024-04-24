module Network.HTTP.Headers.ItemValueSpec (spec) where

import qualified Data.Text.Short as TS
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Mason.Builder as M
import qualified Network.HTTP.Headers.Parsing.Util as P
import qualified Network.HTTP.Headers.Rendering.Util as R
import RFC8941
import Test.Hspec
import Test.Hspec.Hedgehog


itemValueGen :: Gen P.ItemValue
itemValueGen = Gen.choice
  [ P.Integer <$> Gen.integral (Range.linear 0 1_000_000)
  , P.Decimal <$> Gen.realFrac_ (Range.linearFrac 0 1_000_000)
  , P.Boolean <$> Gen.bool
  , P.Binary <$> Gen.bytes (Range.linear 0 1_000)
  , P.String <$> stringGen
  , P.Token <$> tokenGen
  ]

spec :: Spec
spec = do
  describe "ItemValue roundtrip encoding and parsing" $ do
    it "should be able to roundtrip encode and parse" $ hedgehog $ do
      t <- forAll itemValueGen
      tripping t (M.toStrictByteString . R.rfc8941ItemValue) $ \bs -> case P.runParser P.rfc8941ItemValue bs of
        P.OK t' "" -> Right t'
        P.OK _ rest -> Left ("Unconsumed input after parsing ItemValue header: " <> show rest)
        P.Fail -> Left "Failed to parse ItemValue header"
        P.Err err -> Left err

