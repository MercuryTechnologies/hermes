module Network.HTTP.Headers.ETagSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Time
import Data.Either
import Data.Foldable
import Data.Text.Short (fromText)
import Data.Traversable
import FlatParse.Basic
import Mason.Builder (toStrictByteString)
import Network.HTTP.Headers
import Network.HTTP.Headers.ETag
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

entityTagGen :: Gen EntityTag
entityTagGen = Gen.choice
  [ StrongETag . fromText <$> Gen.text (Range.linear 1 64) Gen.alphaNum
  , WeakETag . fromText <$> Gen.text (Range.linear 1 64) Gen.alphaNum
  ]

spec :: Spec
spec = do
  describe "ETag roundtrip encoding and parsing" $ 
    it "should be able to roundtrip encode and parse" $ hedgehog $ do
      t <- forAll entityTagGen
      tripping t (toStrictByteString . renderEntityTag) $ \bs -> case runParser entityTagParser bs of
        OK t' "" -> Right t'
        OK _ rest -> Left ("Unconsumed input after parsing ETag header: " <> show rest)
        Fail -> Left "Failed to parse ETag header"
        Err err -> Left err
