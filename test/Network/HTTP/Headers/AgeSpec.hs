{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Headers.AgeSpec (spec) where
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.List
import Data.List.NonEmpty (NonEmpty(..), fromList)
import FlatParse.Basic
import Mason.Builder (toStrictByteString)
import Network.HTTP.Headers
import Network.HTTP.Headers.Age
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
  it "roundtrips reasonable headers" $ hedgehog $ do
    -- up to ten years in seconds
    ageVal <- forAll $ Gen.word32 $ Range.exponential 0 315_360_000
    tripping (Age ageVal) (toStrictByteString . renderAge) $ \bs -> case runParser parseAge bs of
      OK t' "" -> Right t'
      OK _ rest -> Left ("Unconsumed input after parsing Age header: " <> show rest)
      Fail -> Left "Failed to parse Age header"
      Err err -> Left err
