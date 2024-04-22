module Network.URI where

data URI = URI
  { uriScheme :: !String
  , uriAuthority :: !(Maybe Authority)
  , uriPath :: !String
  , uriQuery :: !String
  , uriFragment :: !String
  } deriving stock (Eq, Show)

data Authority = Authority
  { authorityUserInfo :: !(Maybe UserInfo)
  , authorityHost :: !Host
  , authorityPort :: !(Maybe Port)
  } deriving stock (Eq, Show)

data Host = Host
  { host :: !String
  } deriving stock (Eq, Show)

data Port = Port
  { port :: !String
  } deriving stock (Eq, Show)

data UserInfo = UserInfo
  { userInfoUsername :: !String
  , userInfoPassword :: !(Maybe String)
  } deriving stock (Eq, Show)

-- rfc3986SchemeParser :: ParserT st String String
-- rfc3986SchemeParser = do
--   alpha <- satisfyAscii isAlpha
--   rest <- many (satisfyAscii (`CharSet.member` schemeParserCharset))
--   where
--     schemeParserCharset = alnum <> "+-."

-- rfc3986HierPartParser :: ParserT st String (Maybe Authority, String)
-- rfc3986HierPartParser = do
--   authority <- optional authorityParser
--   path <- pathParser
--   pure (authority, path)

-- rfc3986AuthorityParser :: ParserT st String Authority
-- rfc3986AuthorityParser = do
--   userInfo <- optional userInfoParser
--   host <- hostParser
--   port <- optional portParser
--   pure $ Authority userInfo host port

-- rfc3986UserInfoParser :: ParserT st String UserInfo
-- rfc3986UserInfoParser = do
--   username <- usernameParser
--   password <- optional passwordParser
--   pure $ UserInfo username password

-- rfc3986HostParser :: ParserT st String Host
-- rfc3986HostParser = do
--   host <- hostParser
--   pure $ Host host

-- rfc3986PortParser :: ParserT st String Port
-- rfc3986PortParser = do
--   port <- portParser
--   pure $ Port port

