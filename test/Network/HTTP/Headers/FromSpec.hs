module Network.HTTP.Headers.FromSpec (spec) where

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
import Network.HTTP.Headers.From
import Network.Mailbox
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
  describe "From header parsing" $ do
    it "parses simple email addresses" $ do
      let headers = "user@example.com" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String From
      result `shouldSatisfy` isRight

    it "parses email with display name" $ do
      let headers = "John Doe <john@example.com>" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String From
      result `shouldSatisfy` isRight
      case result of
        Right (From mailbox) ->
          mailboxDisplayName mailbox `shouldBe` Just "John Doe"
        Left _ -> expectationFailure "Expected successful parse"

    it "parses email with quoted display name" $ do
      let headers = "\"John Q. Doe\" <john@example.com>" :| []
      let result = parseFromHeaders defaultHeaderSettings headers :: Either String From
      result `shouldSatisfy` isRight

  describe "From header rendering" $ do
    it "renders simple email" $ do
      let addr = AddrSpec (LocalPartDotAtom "user") (DomainName "example.com" Nothing)
      let from = From $ Mailbox Nothing addr
      let rendered = renderToHeaders defaultHeaderSettings from
      rendered `shouldBe` "user@example.com"

    it "renders email with display name" $ do
      let addr = AddrSpec (LocalPartDotAtom "user") (DomainName "example.com" Nothing)
      let from = From $ Mailbox (Just "John Doe") addr
      let rendered = renderToHeaders defaultHeaderSettings from
      rendered `shouldBe` "John Doe <user@example.com>"

  describe "From header roundtrip" $ do
    it "roundtrips simple addresses" $ do
      let original = "webmaster@example.org"
      let headers = original :| []
      case parseFromHeaders defaultHeaderSettings headers :: Either String From of
        Right from -> do
          let rendered = renderToHeaders defaultHeaderSettings from
          rendered `shouldBe` original
        Left err -> expectationFailure err
