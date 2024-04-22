{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Headers.DateSpec where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Time
import Data.Either
import Data.Foldable
import Data.Traversable
import FlatParse.Basic
import Mason.Builder (toStrictByteString)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

validHttpDateExamples :: [String]
validHttpDateExamples =
  [
    "Sun, 06 Nov 1994 08:49:37 GMT"
  , "Sunday, 06-Nov-94 08:49:37 GMT"
  , "Sun Nov  6 08:49:37 1994"
  , "Sun, 06 Nov 1994 08:49:37 GMT"
  , "Mon, 07 Nov 1994 08:49:37 GMT"
  , "Tue, 08 Nov 1994 08:49:37 GMT"
  , "Wed, 09 Nov 1994 08:49:37 GMT"
  , "Thu, 10 Nov 1994 08:49:37 GMT"
  , "Fri, 11 Nov 1994 08:49:37 GMT"
  , "Sat, 12 Nov 1994 08:49:37 GMT"
  ]

spec :: Spec
spec = do
  describe "Times parse" $ do
    for_ validHttpDateExamples $ \example -> do
      it ("parses " <> example) $ do
        parseFromHeaders @Date defaultHeaderSettings (pure $ C.pack example) `shouldSatisfy` isRight
  describe "Roundtrip encoding and parsing" $ 
    it "should be able to roundtrip encode and parse" $ hedgehog $ do
      t <- forAll $ do
        year <- Gen.integral (Range.linear 1970 9999)
        month <- Gen.integral (Range.linear 1 12)
        let monthDays n = Gen.integral (Range.linear 1 31)
        day <- case month of
          1 -> monthDays 31
          2 -> monthDays (if isLeapYear year then 29 else 28)
          3 -> monthDays 31
          4 -> monthDays 30
          5 -> monthDays 31
          6 -> monthDays 30
          7 -> monthDays 31
          8 -> monthDays 31
          9 -> monthDays 30
          10 -> monthDays 31
          11 -> monthDays 30
          12 -> monthDays 31
          _ -> error "impossible"
        tod <- TimeOfDay <$> 
          Gen.integral (Range.linear 0 23) <*> 
          Gen.integral (Range.linear 0 59) <*> 
          (fromIntegral . round <$> Gen.realFrac_ (Range.linearFrac 0 59))
        pure $ UTCTime (fromGregorian year month day) (timeOfDayToTime tod)
      tripping t (toStrictByteString . renderDate) $ \bs -> case runParser dateParser bs of
        OK t' "" -> Right t'
        OK _ rest -> Left ("Unconsumed input after parsing Date header: " <> show rest)
        Fail -> Left "Failed to parse Date header"
        Err err -> Left err
