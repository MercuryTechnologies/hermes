module Network.MailboxSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import FlatParse.Basic
import Mason.Builder (toStrictByteString)
import Network.Mailbox
import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Generate a simple local part (dot-atom form)
localPartGen :: Gen ShortText
localPartGen = ST.fromText <$> Gen.text (Range.linear 1 32) (Gen.element localChars)
  where
    localChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "!#$%&'*+-/=?^_`{|}~"

-- | Generate a simple domain name
domainGen :: Gen ShortText
domainGen = do
  labels <- Gen.list (Range.linear 1 3) labelGen
  pure $ ST.fromText $ T.intercalate "." (map ST.toText labels)
  where
    labelGen = ST.fromText <$> Gen.text (Range.linear 1 16) Gen.alphaNum

-- | Generate a simple addr-spec
addrSpecGen :: Gen AddrSpec
addrSpecGen = do
  local <- localPartGen
  domain <- domainGen
  pure $ AddrSpec (LocalPartDotAtom local) (DomainName domain Nothing)

-- | Generate a mailbox (optionally with display name)
mailboxGen :: Gen Mailbox
mailboxGen = do
  mDisplayName <- Gen.maybe $ Gen.text (Range.linear 1 32) Gen.alpha
  addr <- addrSpecGen
  pure $ Mailbox mDisplayName addr

spec :: Spec
spec = do
  describe "Mailbox parsing" $ do
    it "parses simple email addresses" $ do
      parseMailbox "user@example.com" `shouldSatisfy` isRight
      parseMailbox "john.doe@example.org" `shouldSatisfy` isRight
      parseMailbox "test+tag@sub.domain.com" `shouldSatisfy` isRight

    it "parses email with display name" $ do
      let result = parseMailbox "John Doe <john@example.com>"
      result `shouldSatisfy` isRight
      case result of
        Right (Mailbox (Just name) _) -> name `shouldBe` "John Doe"
        _ -> expectationFailure "Expected display name"

    it "parses quoted local parts" $ do
      parseMailbox "\"john doe\"@example.com" `shouldSatisfy` isRight

    it "rejects invalid emails" $ do
      parseMailbox "notanemail" `shouldSatisfy` isLeft
      parseMailbox "@example.com" `shouldSatisfy` isLeft
      parseMailbox "user@" `shouldSatisfy` isLeft

  describe "Mailbox rendering" $ do
    it "renders simple addresses" $ do
      let addr = AddrSpec (LocalPartDotAtom "user") (DomainName "example.com" Nothing)
      let mailbox = Mailbox Nothing addr
      toStrictByteString (renderMailbox mailbox) `shouldBe` "user@example.com"

    it "renders addresses with display names" $ do
      let addr = AddrSpec (LocalPartDotAtom "user") (DomainName "example.com" Nothing)
      let mailbox = Mailbox (Just "John Doe") addr
      toStrictByteString (renderMailbox mailbox) `shouldBe` "John Doe <user@example.com>"

  describe "Mailbox roundtrip" $ do
    it "roundtrips simple addresses" $ hedgehog $ do
      addr <- forAll addrSpecGen
      let mailbox = Mailbox Nothing addr
      let rendered = toStrictByteString (renderMailbox mailbox)
      case parseMailbox rendered of
        Right parsed -> mailboxAddrSpec parsed === addr
        Left err -> footnote err >> failure

  describe "IDN support" $ do
    it "decodes punycode domains" $ do
      let result = parseMailbox "user@xn--mnchen-3ya.de"
      result `shouldSatisfy` isRight
      case result of
        Right (Mailbox _ (AddrSpec _ (DomainName ascii mUnicode))) -> do
          ascii `shouldBe` "xn--mnchen-3ya.de"
          mUnicode `shouldBe` Just "münchen.de"
        _ -> expectationFailure "Expected domain with unicode"

    it "renders ASCII form by default" $ do
      let addr = AddrSpec (LocalPartDotAtom "user") (DomainName "xn--mnchen-3ya.de" (Just "münchen.de"))
      let mailbox = Mailbox Nothing addr
      toStrictByteString (renderMailbox mailbox) `shouldBe` "user@xn--mnchen-3ya.de"

    it "renders Unicode form with renderMailboxUnicode" $ do
      let addr = AddrSpec (LocalPartDotAtom "user") (DomainName "xn--mnchen-3ya.de" (Just "münchen.de"))
      let mailbox = Mailbox Nothing addr
      toStrictByteString (renderMailboxUnicode mailbox) `shouldBe` "user@münchen.de"
