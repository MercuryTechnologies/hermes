module Network.HTTP.Headers.AuthorizationSpec where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.Authorization
import Network.HTTP.Headers.HeaderFieldName
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "Authorization" $ do
    it "parses a token credential" $
      let auth = "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
      in parseFromHeaders defaultHeaderSettings (pure auth) `shouldBe` Right (Authorization (Credentials (AuthScheme "Basic") (CredentialToken "QWxhZGRpbjpvcGVuIHNlc2FtZQ==")))

    it "parses a credential with multiple params" $ do
      let auth = "Digest username=\"Mufasa\", realm=\"testrealm@host.com\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", uri=\"/dir/index.html\", qop=auth, nc=00000001, cnonce=\"0a4f113b\", response=\"6629fae49393a05397450978507c4ef1\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\""
      parseFromHeaders defaultHeaderSettings (pure auth) `shouldBe` 
        ( Right $
          Authorization $
            Credentials (AuthScheme "Digest") $
              CredentialParams $ NE.fromList 
                [ ("username", "Mufasa")
                , ("realm", "testrealm@host.com")
                , ("nonce", "dcd98b7102dd2f0e8b11d0f600bfb0c093")
                , ("uri", "/dir/index.html")
                , ("qop", "auth")
                , ("nc", "00000001")
                , ("cnonce", "0a4f113b")
                , ("response", "6629fae49393a05397450978507c4ef1")
                , ("opaque", "5ccc069c403ebaf9f0171e9517f40e41")
                ]
        )

    it "renders a token credential" $
      renderToHeaders defaultHeaderSettings
        (Authorization 
          (Credentials (AuthScheme "Basic") (CredentialToken "QWxhZGRpbjpvcGVuIHNlc2FtZQ=="))) 
          `shouldBe` ["Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="]

    it "renders a credential with multiple params" $ do
      let auth = Authorization $ 
            Credentials 
              (AuthScheme "Digest") 
              (CredentialParams 
                (NE.fromList 
                  [ ("username", "Mufasa")
                  , ("realm", "testrealm@host.com")
                  , ("nonce", "dcd98b7102dd2f0e8b11d0f600bfb0c093")
                  , ("uri", "/dir/index.html")
                  , ("qop", "auth")
                  , ("nc", "00000001")
                  , ("cnonce", "0a4f113b")
                  , ("response", "6629fae49393a05397450978507c4ef1")
                  , ("opaque", "5ccc069c403ebaf9f0171e9517f40e41")
                  ]))
      renderToHeaders defaultHeaderSettings auth `shouldBe` 
        ["Digest username=Mufasa,realm=\"testrealm@host.com\",nonce=dcd98b7102dd2f0e8b11d0f600bfb0c093,uri=\"/dir/index.html\",qop=auth,nc=00000001,cnonce=0a4f113b,response=6629fae49393a05397450978507c4ef1,opaque=5ccc069c403ebaf9f0171e9517f40e41"]


