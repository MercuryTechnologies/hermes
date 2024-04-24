module RFC8941 where
import qualified Data.Text.Short as TS
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Parsing.Util as P
import qualified Network.HTTP.Headers.Rendering.Util as R
import Test.Hspec.Hedgehog (Gen)

tokenGen :: Gen P.RFC8941Token
tokenGen = Gen.mapMaybe 
  (P.mkRFC8941Token . TS.fromText)
  (Gen.text (Range.linear 1 100) (Gen.choice $ (Gen.alphaNum : fmap pure "/:-+.^_`|~!#$%&'*")))

stringGen :: Gen P.RFC8941String
stringGen = Gen.mapMaybe 
  (P.mkRFC8941String . TS.fromText)
  -- TODO, add more characters
  (Gen.text (Range.linear 1 100) Gen.alphaNum)