module Network.HTTP.Headers.Server where

newtype Server = Server { serverProduct :: Product }

data Product = Product
  { productName :: String
  , productVersion :: Maybe String
  }