{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Parsing.Util 
  ( module Network.HTTP.Headers.Parsing.Util
  , module FlatParse.Basic
  ) where

import Control.Applicative (asum)
import Control.Monad.Combinators hiding (skipMany, skipSome, some, many, (<|>), optional)
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import FlatParse.Basic

optionalWhitespace :: ParserT st e ()
optionalWhitespace = ows
{-# INLINE optionalWhitespace #-}

isOWS :: Char -> Bool
isOWS c = c == ' ' || c == '\t'

ows :: ParserT st e ()
ows = skipMany (skipSatisfyAscii isOWS)
{-# INLINE ows #-}

ows1 :: ParserT st e ()
ows1 = skipSome (skipSatisfyAscii isOWS)
{-# INLINE ows1 #-}

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

rfc8941InnerList :: ParserT st e a -> ParserT st e [a]
rfc8941InnerList p = do
  $(char '(')
  ows
  res <- p `sepBy1` ows1
  ows
  $(char ')')
  pure res
{-# INLINE rfc8941InnerList #-}

data ItemValue
  = Integer !Int64
  | Decimal !Milli
  | String !ST.ShortText
  | Token !ST.ShortText
  | Binary !ByteString
  | Boolean !Bool
  deriving stock (Eq, Show)

data ItemValueType t where
  ItemInteger :: ItemValueType Int64
  ItemDecimal :: ItemValueType Milli
  ItemString :: ItemValueType ST.ShortText
  ItemToken :: ItemValueType ST.ShortText
  ItemBinary :: ItemValueType ByteString
  ItemBoolean :: ItemValueType Bool
  ItemOneOf :: NonEmpty (ItemValueType t) -> ItemValueType t
  ItemAny :: ItemValueType ItemValue

rfc8941Integer :: ParserT st e Int64
rfc8941Integer = do
  sign <- ($(char '-') *> pure negate) <|> pure id
  res <- isolate 15 anyAsciiDecimalWord
  pure $ sign $ fromIntegral res
{-# INLINE rfc8941Integer #-}

rfc8941Decimal :: ParserT st e Milli
rfc8941Decimal = do
  sign <- ($(char '-') *> pure negate) <|> pure id
  decimalPart <- isolate 12 anyAsciiDecimalInteger
  $(char '.')
  withSpan (isolate 3 anyAsciiDecimalInteger) $ \fractionalPart (Span (Pos start) (Pos end)) -> do
    let fractionalPart' = case end - start of
          1 -> fractionalPart * 100
          2 -> fractionalPart * 10
          3 -> fractionalPart
          _ -> error "Impossible"
    pure $ MkFixed $ sign (decimalPart * 1000 + fractionalPart')
{-# INLINE rfc8941Decimal #-}

rfc8941Token :: ParserT st e ST.ShortText
rfc8941Token = withByteString tokenParser $ \_ bs -> do
  pure $! STU.fromByteStringUnsafe bs
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

rfc8941String :: ParserT st e ST.ShortText
rfc8941String = quotedString
{-# INLINE rfc8941String #-}

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
itemValueTypeParser (ItemOneOf items) = asum $ fmap itemValueTypeParser items
{-# INLINE itemValueTypeParser #-}

rfc8941Parameter :: ItemValueType t -> ParserT st String (ST.ShortText, Maybe t)
rfc8941Parameter t = do
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
{-# INLINE rfc8941Parameter #-}

rfc8941Parameters :: ParserT st String [(ST.ShortText, Maybe ItemValue)]
rfc8941Parameters = many (rfc8941Parameter ItemAny)
{-# INLINE rfc8941Parameters #-}
