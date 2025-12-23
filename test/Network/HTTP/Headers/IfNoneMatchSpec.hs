module Network.HTTP.Headers.IfNoneMatchSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import FlatParse.Basic
import Mason.Builder (toStrictByteString)
import Network.HTTP.Headers
import Network.HTTP.Headers.ETag
import Network.HTTP.Headers.IfNoneMatch
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
  describe "If-None-Match header parsing" $ do
    it "parses wildcard" $ do
      let headers = "*" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch
      result `shouldBe` Right IfNoneMatchAnyRepresentation

    it "parses single strong ETag" $ do
      let headers = "\"abc123\"" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch
      result `shouldSatisfy` isRight
      case result of
        Right (IfNoneMatchEntityTags tags) ->
          NE.head tags `shouldBe` StrongETag "abc123"
        _ -> expectationFailure "Expected entity tags"

    it "parses single weak ETag" $ do
      let headers = "W/\"abc123\"" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch
      result `shouldSatisfy` isRight
      case result of
        Right (IfNoneMatchEntityTags tags) ->
          NE.head tags `shouldBe` WeakETag "abc123"
        _ -> expectationFailure "Expected entity tags"

    it "parses multiple ETags" $ do
      let headers = "\"tag1\", \"tag2\", W/\"tag3\"" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch
      result `shouldSatisfy` isRight
      case result of
        Right (IfNoneMatchEntityTags tags) ->
          length (NE.toList tags) `shouldBe` 3
        _ -> expectationFailure "Expected entity tags"

    it "parses ETags from multiple header values" $ do
      let headers = "\"tag1\"" :| ["\"tag2\""]
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch
      result `shouldSatisfy` isRight
      case result of
        Right (IfNoneMatchEntityTags tags) ->
          length (NE.toList tags) `shouldBe` 2
        _ -> expectationFailure "Expected entity tags"

  describe "If-None-Match header rendering" $ do
    it "renders wildcard" $ do
      let rendered = renderToHeaders defaultHeaderSettings IfNoneMatchAnyRepresentation
      rendered `shouldBe` "*"

    it "renders single ETag" $ do
      let header = IfNoneMatchEntityTags (StrongETag "abc123" :| [])
      let rendered = renderToHeaders defaultHeaderSettings header
      rendered `shouldBe` "\"abc123\""

    it "renders multiple ETags" $ do
      let header = IfNoneMatchEntityTags (StrongETag "tag1" :| [WeakETag "tag2"])
      let rendered = renderToHeaders defaultHeaderSettings header
      rendered `shouldBe` "\"tag1\", W/\"tag2\""

  describe "If-None-Match header roundtrip" $ do
    it "roundtrips wildcard" $ do
      let original = "*"
      let headers = original :| []
      case parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch of
        Right header -> do
          let rendered = renderToHeaders defaultHeaderSettings header
          rendered `shouldBe` original
        Left err -> expectationFailure err

    it "roundtrips entity tags" $ do
      let original = "\"abc\", W/\"def\""
      let headers = original :| []
      case parseFromHeaders defaultHeaderSettings headers :: Either String IfNoneMatch of
        Right header -> do
          let rendered = renderToHeaders defaultHeaderSettings header
          rendered `shouldBe` original
        Left err -> expectationFailure err
