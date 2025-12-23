module Network.HTTP.Headers.ServerSpec (spec) where

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
import Network.HTTP.Headers.Server
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generate a product token
productGen :: Gen Product
productGen = do
  name <- ST.fromText <$> Gen.text (Range.linear 1 16) Gen.alphaNum
  mVersion <- Gen.maybe $ ST.fromText <$> Gen.text (Range.linear 1 8) Gen.alphaNum
  pure $ Product name mVersion

-- | Generate a Server header value
serverGen :: Gen Server
serverGen = do
  first <- productGen
  -- Keep it simple for now - just one product
  pure $ Server first []

spec :: Spec
spec = do
  describe "Server header parsing" $ do
    it "parses simple server strings" $ do
      let result = runParser serverParser "Apache"
      case result of
        OK server "" -> productName (firstServerProduct server) `shouldBe` "Apache"
        _ -> expectationFailure "Failed to parse"

    it "parses server with version" $ do
      let result = runParser serverParser "Apache/2.4.41"
      case result of
        OK server "" -> do
          productName (firstServerProduct server) `shouldBe` "Apache"
          productVersion (firstServerProduct server) `shouldBe` Just "2.4.41"
        _ -> expectationFailure "Failed to parse"

    it "parses multiple products" $ do
      let result = runParser serverParser "Apache/2.4.41 OpenSSL/1.1.1"
      case result of
        OK server "" -> do
          productName (firstServerProduct server) `shouldBe` "Apache"
          length (remainingServerDefinition server) `shouldBe` 1
        _ -> expectationFailure "Failed to parse"

    it "parses with comments" $ do
      let result = runParser serverParser "Apache/2.4.41 (Ubuntu)"
      case result of
        OK server "" -> do
          productName (firstServerProduct server) `shouldBe` "Apache"
          length (remainingServerDefinition server) `shouldBe` 1
        _ -> expectationFailure "Failed to parse"

  describe "Server header rendering" $ do
    it "renders simple server" $ do
      let server = Server (Product "nginx" Nothing) []
      toStrictByteString (renderServer server) `shouldBe` "nginx"

    it "renders server with version" $ do
      let server = Server (Product "nginx" (Just "1.19.0")) []
      toStrictByteString (renderServer server) `shouldBe` "nginx/1.19.0"

    it "renders multiple products" $ do
      let server = Server (Product "Apache" (Just "2.4")) [Right (Product "OpenSSL" (Just "1.1"))]
      toStrictByteString (renderServer server) `shouldBe` "Apache/2.4 OpenSSL/1.1"

  describe "Server header roundtrip" $ do
    it "roundtrips simple servers" $ hedgehog $ do
      server <- forAll serverGen
      let rendered = toStrictByteString (renderServer server)
      case runParser serverParser rendered of
        OK parsed "" -> firstServerProduct parsed === firstServerProduct server
        _ -> failure
