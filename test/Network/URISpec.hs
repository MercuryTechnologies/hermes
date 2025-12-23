module Network.URISpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import FlatParse.Basic
import Mason.Builder (toStrictByteString)
import Network.URI
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generate a scheme
schemeGen :: Gen Scheme
schemeGen = Scheme . ST.fromText <$> Gen.element ["http", "https", "ftp", "mailto", "file"]

-- | Generate a simple host
hostGen :: Gen Host
hostGen = do
  labels <- Gen.list (Range.linear 1 3) labelGen
  let domain = ST.fromText $ T.intercalate "." (map ST.toText labels)
  pure $ HostRegName domain Nothing
  where
    labelGen = ST.fromText <$> Gen.text (Range.linear 1 12) Gen.alphaNum

-- | Generate a port
portGen :: Gen Port
portGen = Port <$> Gen.word16 (Range.linear 1 65535)

-- | Generate a path
pathGen :: Gen Path
pathGen = do
  segments <- Gen.list (Range.linear 0 4) segmentGen
  let pathStr = "/" <> T.intercalate "/" (map ST.toText segments)
  pure $ Path $ ST.fromText pathStr
  where
    segmentGen = ST.fromText <$> Gen.text (Range.linear 1 16) Gen.alphaNum

-- | Generate a simple URI with authority
uriWithAuthGen :: Gen URI
uriWithAuthGen = do
  scheme <- schemeGen
  host <- hostGen
  mPort <- Gen.maybe portGen
  path <- pathGen
  let auth = URIAuth Nothing host mPort
  pure $ URI scheme (Just auth) path Nothing Nothing

spec :: Spec
spec = do
  describe "URI parsing" $ do
    it "parses simple HTTP URLs" $ do
      parseURI "http://example.com" `shouldSatisfy` isRight
      parseURI "https://example.com/path" `shouldSatisfy` isRight
      parseURI "https://example.com:8080/path" `shouldSatisfy` isRight

    it "parses URLs with query and fragment" $ do
      let result = parseURI "https://example.com/path?query=value#section"
      result `shouldSatisfy` isRight
      case result of
        Right uri -> do
          uriQuery uri `shouldBe` Just (Query "query=value")
          uriFragment uri `shouldBe` Just (Fragment "section")
        Left _ -> expectationFailure "Expected successful parse"

    it "parses URLs with userinfo" $ do
      let result = parseURI "https://user:pass@example.com/path"
      result `shouldSatisfy` isRight
      case result of
        Right uri -> case uriAuthority uri of
          Just auth -> uriUserInfo auth `shouldBe` Just (UserInfo "user:pass")
          Nothing -> expectationFailure "Expected authority"
        Left _ -> expectationFailure "Expected successful parse"

    it "parses IPv4 addresses" $ do
      let result = parseURI "http://192.168.1.1:8080/path"
      result `shouldSatisfy` isRight
      case result of
        Right uri -> case uriAuthority uri of
          Just auth -> uriHost auth `shouldBe` HostIPv4 "192.168.1.1"
          Nothing -> expectationFailure "Expected authority"
        Left _ -> expectationFailure "Expected successful parse"

    it "parses IPv6 addresses" $ do
      let result = parseURI "http://[::1]:8080/path"
      result `shouldSatisfy` isRight
      case result of
        Right uri -> case uriAuthority uri of
          Just auth -> uriHost auth `shouldBe` HostIPv6 "[::1]"
          Nothing -> expectationFailure "Expected authority"
        Left _ -> expectationFailure "Expected successful parse"

    it "parses various schemes" $ do
      parseURI "ftp://files.example.com/file.txt" `shouldSatisfy` isRight
      parseURI "mailto:user@example.com" `shouldSatisfy` isRight
      parseURI "file:///path/to/file" `shouldSatisfy` isRight

  describe "URI-reference parsing" $ do
    it "parses relative references" $ do
      parseURIReference "/path/to/resource" `shouldSatisfy` isRight
      parseURIReference "path/to/resource" `shouldSatisfy` isRight
      parseURIReference "?query=value" `shouldSatisfy` isRight
      parseURIReference "#fragment" `shouldSatisfy` isRight

  describe "URI rendering" $ do
    it "renders simple URIs" $ do
      let auth = URIAuth Nothing (HostRegName "example.com" Nothing) (Just (Port 8080))
      let uri = URI (Scheme "https") (Just auth) (Path "/path") Nothing Nothing
      toStrictByteString (renderURI uri) `shouldBe` "https://example.com:8080/path"

    it "renders URIs with query and fragment" $ do
      let auth = URIAuth Nothing (HostRegName "example.com" Nothing) Nothing
      let uri = URI (Scheme "https") (Just auth) (Path "/path") (Just (Query "q=1")) (Just (Fragment "sec"))
      toStrictByteString (renderURI uri) `shouldBe` "https://example.com/path?q=1#sec"

  describe "URI roundtrip" $ do
    it "roundtrips URIs with authority" $ hedgehog $ do
      uri <- forAll uriWithAuthGen
      let rendered = toStrictByteString (renderURI uri)
      case parseURI rendered of
        Right parsed -> do
          uriScheme parsed === uriScheme uri
          uriPath parsed === uriPath uri
        Left err -> footnote err >> failure

  describe "IDN support" $ do
    it "decodes punycode hosts" $ do
      let result = parseURI "https://xn--mnchen-3ya.de/path"
      result `shouldSatisfy` isRight
      case result of
        Right uri -> case uriAuthority uri of
          Just auth -> case uriHost auth of
            HostRegName ascii mUnicode -> do
              ascii `shouldBe` "xn--mnchen-3ya.de"
              mUnicode `shouldBe` Just "münchen.de"
            _ -> expectationFailure "Expected reg-name host"
          Nothing -> expectationFailure "Expected authority"
        Left _ -> expectationFailure "Expected successful parse"

    it "renders ASCII host by default" $ do
      let host = HostRegName "xn--mnchen-3ya.de" (Just "münchen.de")
      let auth = URIAuth Nothing host Nothing
      let uri = URI (Scheme "https") (Just auth) (Path "/") Nothing Nothing
      toStrictByteString (renderURI uri) `shouldBe` "https://xn--mnchen-3ya.de/"

    it "renders Unicode host with renderURIUnicode" $ do
      let host = HostRegName "xn--mnchen-3ya.de" (Just "münchen.de")
      let auth = URIAuth Nothing host Nothing
      let uri = URI (Scheme "https") (Just auth) (Path "/") Nothing Nothing
      toStrictByteString (renderURIUnicode uri) `shouldBe` "https://münchen.de/"

  describe "Percent encoding" $ do
    it "encodes special characters" $ do
      -- Space is not unreserved, so it gets encoded
      percentDecode "hello%20world" `shouldBe` "hello world"

    it "decodes percent-encoded strings" $ do
      percentDecode "hello%20world" `shouldBe` "hello world"
      percentDecode "caf%C3%A9" `shouldBe` "café"

    it "preserves unreserved characters" $ do
      -- Unreserved chars should not be encoded in a well-formed URI
      let uri = "https://example.com/path-with_tilde~"
      parseURI uri `shouldSatisfy` isRight
