{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.Headers.Authorization where
import Control.Monad.Combinators.NonEmpty
import Data.ByteString (ByteString)
import Data.Text.Short (ShortText)
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import FlatParse.Basic
import Network.HTTP.Headers.Parsing.Util 
import Network.HTTP.Headers.Rendering.Util
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Mason.Builder as M

newtype AuthScheme = AuthScheme ShortText
  deriving stock (Eq, Show)

data CredentialContents
  = CredentialToken ByteString
  | CredentialParams (NE.NonEmpty (ShortText, ShortText))
  deriving stock (Eq, Show)

data Credentials = Credentials
  { scheme :: !AuthScheme
  , contents :: !CredentialContents
  } deriving stock (Eq, Show)

credentialsParser :: ParserT st e Credentials
credentialsParser = do
  scheme <- AuthScheme <$> rfc9110Token
  skipSome $(char ' ')
  contents <- paramCredential <|> tokenCredential
  pure $ Credentials scheme contents
  where
    tokenCredential = CredentialToken <$> rfc7230Token68Parser
    paramCredential = CredentialParams <$> credentialsP
    credentialsP = do
      rfc8941List1 authParamP
    authParamP = do
      key <- rfc9110Token
      bws
      $(char '=')
      bws
      val <- quotedString <|> rfc9110Token
      pure (key, val)
    rfc7230Token68Parser = byteStringOf (skipSome $ skipSatisfyAscii (`CharSet.member` token68Chars))
      where
        token68Chars = CharSet.fromList $ ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> "-._~+/="

renderCredentials :: Credentials -> M.Builder
renderCredentials = \case
  Credentials (AuthScheme scheme) (CredentialToken token) -> shortText scheme <> M.char7 ' ' <> M.byteString token
  Credentials (AuthScheme scheme) (CredentialParams params) -> shortText scheme <> M.char7 ' ' <> M.intersperse (M.char7 ',') (NE.toList $ fmap renderParam params)
  where
    renderParam (key, val) = shortText key <> M.char7 '=' <> shortText val

newtype Authorization = Authorization { authorizationCredentials :: Credentials }
  deriving stock (Eq, Show)

instance KnownHeader Authorization where
  type ParseFailure Authorization = String

  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser credentialsParser header of
      OK creds "" -> Right $ Authorization creds
      OK _ rest -> Left $ "Unconsumed input after parsing Authorization header: " <> show rest
      Fail -> Left "Failed to parse Authorization header"
      Err err -> Left err

  renderToHeaders _ = pure . M.toStrictByteString . renderCredentials . authorizationCredentials

  headerName _ = hAuthorization

