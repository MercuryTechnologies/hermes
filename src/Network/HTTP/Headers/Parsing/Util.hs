{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Parsing.Util 
  ( module Network.HTTP.Headers.Parsing.Util
  , module FlatParse.Basic
  ) where

import Control.Applicative (asum)
import Control.Monad.Combinators hiding (skipMany, skipSome, some, many, (<|>), optional)
import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Char
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii
import Data.Fixed
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import FlatParse.Basic
import FlatParse.Basic.Base
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))

optionalWhitespace :: ParserT st e ()
optionalWhitespace = ows
{-# INLINE optionalWhitespace #-}

isOWS :: Char -> Bool
isOWS c = c == ' ' || c == '\t'

-- | Optional whitespace (space or tab).
ows :: ParserT st e ()
ows = skipMany (skipSatisfyAscii isOWS)
{-# INLINE ows #-}

-- | Required whitespace (space or tab).
rws :: ParserT st e ()
rws = skipSome (skipSatisfyAscii isOWS)
{-# INLINE rws #-}

-- | "Bad whitespace" (space or tab).
--
-- Used where a grammar allows optional whitespace, but only for historical reasons.
bws :: ParserT st e ()
bws = ows
{-# INLINE bws #-}

shortASCIIFromParser_ :: ParserT st e a -> ParserT st e ST.ShortText
shortASCIIFromParser_ p = withByteString p $ \_res s -> pure $ STU.fromByteStringUnsafe s

tokenCharSet :: CharSet
tokenCharSet = alnum <> "-+.^_`|~!#$%&'*"

rfc9110Token :: ParserT st e ST.ShortText
rfc9110Token = shortASCIIFromParser_ $ some (satisfyAscii (`CharSet.member` tokenCharSet)) 
{-# INLINE rfc9110Token #-}

rfc9110TokenBS :: ParserT st e ByteString
rfc9110TokenBS = byteStringOf $ some (satisfyAscii (`CharSet.member` tokenCharSet)) 
{-# INLINE rfc9110TokenBS #-}

fieldName :: ParserT st e ST.ShortText
fieldName = rfc9110Token
{-# INLINE fieldName #-}

obsTextCharSet :: CharSet
obsTextCharSet = CharSet.fromList ['\x80'..'\xFF']

quotedPairCharSet :: CharSet
quotedPairCharSet = "\t !-~" <> obsTextCharSet

quotedCharSet :: CharSet
quotedCharSet = 
  "\t \x21" <> 
  CharSet.fromList ['\x2A'..'\x5B'] <> 
  CharSet.fromList ['\x5D'..'\x7E'] <> 
  obsTextCharSet

-- quotedChar :: (Char -> Either String Bool) -> Either String Bool -> Char -> Maybe (Either String Bool)
-- quotedChar _ (Right False) '"' = Nothing
-- quotedChar _ (Right False) '\\' = Just $ Right True
-- quotedChar _ (Right True) c = if inClass "\t !-~\x80-\xFF" c
--   then Just (Right False)
--   else Just (Left $ "Invalid escape character in quoted string: " ++ show (w2c c))
-- quotedChar f (Right False) c = Just $ f c
-- quotedChar _ (Left c) _ = Nothing

quotedString :: ParserT st e ST.ShortText
quotedString = between 
  $(char '"') 
  $(char '"') 
  (ST.pack <$> many (unescapedChar <|> escapedChar))
  where
    unescapedChar = satisfyAscii (`CharSet.member` quotedCharSet)
    escapedChar = do
      $(char '\\')
      satisfyAscii (`CharSet.member` quotedPairCharSet)
{-# INLINE quotedString #-}

commentCharSet :: CharSet
commentCharSet = 
  "\t " <> 
  CharSet.fromList ['\x21'..'\x27'] <> 
  CharSet.fromList ['\x2A'..'\x5B'] <> 
  CharSet.fromList ['\x5D'..'\x7E'] <> 
  obsTextCharSet

newtype Comment = Comment { fromComment :: Text }
  deriving stock (Eq, Show)

comment :: ParserT st e Text
comment = between 
  $(char '(') 
  $(char ')') 
  (Text.pack <$> many (unescapedChar <|> escapedChar))
  where
    unescapedChar = satisfyAscii (`CharSet.member` quotedCharSet)
    escapedChar = do
      $(char '\\')
      satisfyAscii (`CharSet.member` quotedPairCharSet)
{-# INLINE comment #-}

rfc8941List :: ParserT st e a -> ParserT st e [a]
rfc8941List p = p `sepBy` (ows *> $(char ',') *> ows)
{-# INLINE rfc8941List #-}

rfc8941List1 :: ParserT st e a -> ParserT st e (NonEmpty a)
rfc8941List1 p = p `NE.sepBy1` (ows *> $(char ',') *> ows)
{-# INLINE rfc8941List1 #-}

rfc8941InnerList :: ParserT st e a -> ParserT st e (NonEmpty a)
rfc8941InnerList p = do
  $(char '(')
  ows
  res <- p `NE.sepBy1` rws
  ows
  $(char ')')
  pure res
{-# INLINE rfc8941InnerList #-}

data ItemValue
  = Integer !Int
  | Decimal !Milli
  | String !RFC8941String
  | Token !RFC8941Token
  | Binary !ByteString
  | Boolean !Bool
  deriving stock (Eq, Show)

data ItemValueType t where
  ItemInteger :: ItemValueType Int
  ItemDecimal :: ItemValueType Milli
  ItemString :: ItemValueType RFC8941String
  ItemToken :: ItemValueType RFC8941Token
  ItemBinary :: ItemValueType ByteString
  ItemBoolean :: ItemValueType Bool
  ItemAny :: ItemValueType ItemValue

instance Lift (ItemValueType t) where
  lift ItemInteger = [| ItemInteger |]
  lift ItemDecimal = [| ItemDecimal |]
  lift ItemString = [| ItemString |]
  lift ItemToken = [| ItemToken |]
  lift ItemBinary = [| ItemBinary |]
  lift ItemBoolean = [| ItemBoolean |]
  lift ItemAny = [| ItemAny |]

rfc8941Integer :: ParserT st e Int
rfc8941Integer = do
  baseVal `notFollowedBy` $(char '.')
  where
    baseVal = do
      sign <- ($(char '-') *> pure negate) <|> pure id
      res <- anyAsciiDecimalWord
      pure $ sign $ fromIntegral res
{-# INLINE rfc8941Integer #-}

rfc8941Decimal :: ParserT st String Milli
rfc8941Decimal = do
  sign <- branch $(char '-') (pure negate) (pure id)
  decimalPart <- anyAsciiDecimalInteger
  fracPart <- branch $(char '.') 
    ( do
      startingZeros <- length <$> many (satisfyAscii (== '0'))
      withOption 
        anyAsciiDecimalInteger
        (\fractionalPart -> pure (fractionalPart * 10 ^ negate startingZeros))
        (pure 0)
    )
    (pure 0)
  pure $ MkFixed $ sign (decimalPart * 1000 + fracPart)
{-# INLINE rfc8941Decimal #-}

newtype RFC8941Token = RFC8941Token { unsafeToRFC8941Token :: ST.ShortText }
  deriving stock (Eq, Show)

mkRFC8941Token :: ST.ShortText -> Maybe RFC8941Token
mkRFC8941Token t = case runParser rfc8941Token (ST.toByteString t) of
  OK x "" -> Just x
  _ -> Nothing

rfc8941Token :: ParserT st e RFC8941Token
rfc8941Token = withByteString tokenParser $ \_ bs -> do
  pure $ RFC8941Token $! STU.fromByteStringUnsafe bs
  where
    tokenParser = do
      skipSatisfyAscii (\c -> c == '*' || c `CharSet.member` alpha)
      skipMany (skipSatisfyAscii (`CharSet.member` rfc8941TokenCharSet))
    rfc8941TokenCharSet = "/:" <> tokenCharSet
{-# INLINE rfc8941Token #-}

rfc8941Boolean :: ParserT st e Bool
rfc8941Boolean = $(switch [| case _ of
  "?0" -> pure True
  "?1" -> pure False |])
{-# INLINE rfc8941Boolean #-}

rfc8941Binary :: ParserT st String ByteString
rfc8941Binary = do
  $(char ':')
  withByteString (many (satisfyAscii (`CharSet.member` base64CharSet))) $ \_ bs -> do
    case convertFromBase Base64 bs of
      Left e -> err e
      Right ok -> pure ok
  where
    base64CharSet = alpha <> digit <> "+/="
{-# INLINE rfc8941Binary #-}

newtype RFC8941String = RFC8941String { unsafeToRFC8941String :: ST.ShortText }
  deriving stock (Eq, Show)

rfc8941String :: ParserT st e RFC8941String
rfc8941String = RFC8941String <$> quotedString
{-# INLINE rfc8941String #-}

mkRFC8941String :: ST.ShortText -> Maybe RFC8941String
mkRFC8941String t = if ST.all (`CharSet.member` (quotedCharSet <> quotedPairCharSet)) t
  then Just $ RFC8941String t
  else Nothing

rfc8941ItemValue :: ParserT st String ItemValue
rfc8941ItemValue = asum
  [ (Integer <$> rfc8941Integer)
  , (Decimal <$> rfc8941Decimal)
  , (String <$> rfc8941String)
  , (Token <$> rfc8941Token)
  , (Binary <$> rfc8941Binary)
  , (Boolean <$> rfc8941Boolean)
  ]

itemValueTypeParser :: ItemValueType t -> ParserT st String t
itemValueTypeParser ItemInteger = rfc8941Integer
itemValueTypeParser ItemDecimal = rfc8941Decimal
itemValueTypeParser ItemString = rfc8941String
itemValueTypeParser ItemToken = rfc8941Token
itemValueTypeParser ItemBinary = rfc8941Binary
itemValueTypeParser ItemBoolean = rfc8941Boolean
itemValueTypeParser ItemAny = rfc8941ItemValue
{-# INLINE itemValueTypeParser #-}

anyRfc8941Parameter :: ItemValueType t -> ParserT st String (ST.ShortText, Maybe t)
anyRfc8941Parameter t = do
  $(char ';')
  ows
  key <- paramKey
  paramValue <- optional $ do
    $(char '=')
    itemValueTypeParser t
  pure (key, paramValue)
  where
    lcalpha = CharSet.fromList ['a'..'z']
    star = CharSet.singleton '*'
    firstKeyChar = lcalpha <> star
    keyChar = lcalpha <> digit <> "_-.*"
    paramKey = withByteString (skipSatisfyAscii (`CharSet.member` firstKeyChar) *> skipMany (skipSatisfyAscii (`CharSet.member` keyChar))) $ \_ bs -> do
      pure $ STU.fromByteStringUnsafe bs
{-# INLINE anyRfc8941Parameter #-}

knownRfc8941Parameter :: Q Exp -> ItemValueType t -> Q Exp -- ParserT st String (Maybe t)
knownRfc8941Parameter k t = do
  [| do
    $(char ';')
    ows
    key <- $(k)
    paramValue <- optional $ do
      $(char '=')
      itemValueTypeParser t
    pure paramValue |]
  where
    lcalpha = CharSet.fromList ['a'..'z']
    star = CharSet.singleton '*'
    firstKeyChar = lcalpha <> star
    keyChar = lcalpha <> digit <> "_-.*"
    paramKey = withByteString (skipSatisfyAscii (`CharSet.member` firstKeyChar) *> skipMany (skipSatisfyAscii (`CharSet.member` keyChar))) $ \_ bs -> do
      pure $ STU.fromByteStringUnsafe bs
{-# INLINE knownRfc8941Parameter #-}

knownRfc8941WithRequiredKeyParser :: Q Exp -> Q Exp -> Q Exp -- ParserT st String (Maybe t)
knownRfc8941WithRequiredKeyParser k t = do
  [| do
    $(char ';')
    ows
    key <- $(k)
    $(char '=')
    $(t) |]
{-# INLINE knownRfc8941WithRequiredKeyParser #-}

knownRfc8941WithNoValueParser :: Q Exp -> Q Exp -- ParserT st String (Maybe t)
knownRfc8941WithNoValueParser k = do
  [| do
    $(char ';')
    ows
    $(k) |]
{-# INLINE knownRfc8941WithNoValueParser #-}

rfc8941Parameters :: ParserT st String [(ST.ShortText, Maybe ItemValue)]
rfc8941Parameters = many (anyRfc8941Parameter ItemAny)
{-# INLINE rfc8941Parameters #-}

-- | Run the parser, if an error is thrown, handle it with the given function.
embedError :: ParserT st e b -> (e -> ParserT st e' b) -> ParserT st e' b
embedError (ParserT f) hdl = ParserT $ \fp eob s st -> case f fp eob s st of
  Err# st' e -> case hdl e of
    ParserT g -> g fp eob s st'
  OK# st' x a -> OK# st' x a
  Fail# st' -> Fail# st'
{-# inline embedError #-}
