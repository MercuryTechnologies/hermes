{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.WWWAuthenticate
  ( WWWAuthenticate (..)
  , AuthChallenge (..)
  , wwwAuthenticateParser
  , renderWWWAuthenticate
  ) where

import Control.Monad.Combinators (sepBy)
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import qualified Mason.Builder as M
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hWWWAuthenticate)
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)

-- | Authentication challenge consisting of scheme and challenge parameters
data AuthChallenge = AuthChallenge
  { challengeScheme :: !ST.ShortText
  , challengeParams :: !ST.ShortText  -- ^ Rest of the challenge as raw text
  } deriving stock (Eq, Show)

-- | WWW-Authenticate header containing one or more authentication challenges
newtype WWWAuthenticate = WWWAuthenticate { authChallenges :: [AuthChallenge] }
  deriving stock (Eq, Show)

instance KnownHeader WWWAuthenticate where
  type ParseFailure WWWAuthenticate = String
  type Cardinality WWWAuthenticate = 'ZeroOrMore
  type Direction WWWAuthenticate = 'Response

  parseFromHeaders _ headers = do
    challenges <- traverse parseSingleChallenge (NE.toList headers)
    pure $ WWWAuthenticate $ concat challenges

  renderToHeaders _ (WWWAuthenticate challenges) = map (M.toStrictByteString . renderAuthChallenge) challenges

  headerName _ = hWWWAuthenticate

parseSingleChallenge :: B.ByteString -> Either String [AuthChallenge]
parseSingleChallenge header = case runParser challengesParser header of
  OK challenges "" -> Right challenges
  OK _ rest -> Left $ "Unconsumed input after parsing WWW-Authenticate challenges: " <> show rest
  Fail -> Left "Failed to parse WWW-Authenticate header"
  Err err -> Left err

challengesParser :: ParserT st String [AuthChallenge]
challengesParser = authChallengeParser `sepBy` (ows *> $(char ',') *> ows)

authChallengeParser :: ParserT st String AuthChallenge
authChallengeParser = do
  scheme <- rfc9110Token
  rws
  params <- takeRestShortText
  pure $ AuthChallenge scheme params

wwwAuthenticateParser :: ParserT st String WWWAuthenticate
wwwAuthenticateParser = WWWAuthenticate <$> challengesParser

renderAuthChallenge :: AuthChallenge -> M.Builder
renderAuthChallenge (AuthChallenge scheme params) =
  shortText scheme <> " " <> shortText params

renderWWWAuthenticate :: WWWAuthenticate -> M.Builder
renderWWWAuthenticate (WWWAuthenticate challenges) =
  M.intersperse ", " $ map renderAuthChallenge challenges
