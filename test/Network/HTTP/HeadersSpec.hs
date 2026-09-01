module Network.HTTP.HeadersSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.CacheControl
import Network.HTTP.Headers.HeaderFieldName
import Test.Hspec

spec :: Spec
spec = describe "HeaderMap" $ do
  -- One constant suffices since all predefined names share the same construction path.
  it "lookupRawHeader finds a predefined name inserted via headerMapFromList" $
    lookupRawHeader hCacheControl (headerMapFromList [("cache-control", "some value")])
      `shouldBe` Just ("some value" :| [])

  it "lookupRawHeader finds names that are not predefined constants" $
    lookupRawHeader "x-custom-header" (headerMapFromList [("x-custom-header", "some value")])
      `shouldBe` Just ("some value" :| [])

  it "lookupRawHeader returns Nothing for an absent name" $
    lookupRawHeader hAge (headerMapFromList [("vary", "accept-encoding")])
      `shouldBe` Nothing

  it "lookupRawHeader returns repeated values in insertion order" $
    lookupRawHeader hVary (headerMapFromList [("vary", "accept"), ("vary", "accept-encoding")])
      `shouldBe` Just ("accept" :| ["accept-encoding"])

  it "headerMapToList returns repeated values in insertion order" $
    headerMapToList (headerMapFromList [("vary", "accept"), ("vary", "accept-encoding")])
      `shouldBe` [("vary", "accept"), ("vary", "accept-encoding")]

  it "lookupRawHeader returns incrementally inserted values in insertion order" $ do
    let hMap = insertRawHeader hVary "accept-encoding" (insertRawHeader hVary "accept" (headerMapFromList []))
    lookupRawHeader hVary hMap `shouldBe` Just ("accept" :| ["accept-encoding"])

  it "deleteHeader removes a name" $
    lookupRawHeader hVary (deleteHeader hVary (headerMapFromList [("vary", "accept")]))
      `shouldBe` Nothing

  it "lookupHeader parses a known header inserted via headerMapFromList" $
    lookupHeader @CacheControl (headerMapFromList [("cache-control", "public, max-age=3600")])
      `shouldBe` Right (Just (CacheControl (Public :| [MaxAge 3600])))

  it "lookupHeader is insensitive to the inserted name's case" $
    lookupHeader @CacheControl (headerMapFromList [("Cache-Control", "no-store")])
      `shouldBe` Right (Just (CacheControl (NoStore :| [])))
