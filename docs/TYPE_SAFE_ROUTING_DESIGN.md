# Type-Safe Routing Design for Hermes

## Executive Summary

This document specifies the design for expanding Hermes into a full type-safe HTTP framework comparable to [Akka HTTP](https://doc.akka.io/docs/akka-http/current/routing-dsl/overview.html) and [Servant](https://www.servant.dev/). The goal is to provide:

1. **Type-safe routing** - Compile-time verification that routes are well-formed
2. **Bidirectional derivation** - Generate both servers and clients from the same specification
3. **Composable directives** - Build complex routes from simple, reusable building blocks
4. **Integration with existing Hermes primitives** - Leverage the existing `KnownHeader`, `Method`, `StatusCode` infrastructure

## Background: Akka HTTP vs Servant

### Akka HTTP Approach (Scala)

Akka HTTP uses a **directives-based DSL** where routes are composed at runtime:

```scala
type Route = RequestContext => Future[RouteResult]

val route =
  pathPrefix("users") {
    concat(
      pathEnd {
        get { complete(getUsers()) }
      },
      path(IntNumber) { userId =>
        get { complete(getUser(userId)) }
      }
    )
  }
```

**Key characteristics:**
- Routes are functions: `RequestContext => Future[RouteResult]`
- Directives extract values and pass them to inner routes
- Composition via `concat` (try alternatives) and nesting
- Type safety comes from extraction types, not the route structure itself
- Runtime route matching

### Servant Approach (Haskell)

Servant uses **type-level API definitions** where routes exist entirely in the type system:

```haskell
type API = "users" :> Get '[JSON] [User]
       :<|> "users" :> Capture "id" Int :> Get '[JSON] User
       :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
```

**Key characteristics:**
- API is a type, not a value
- Servers and clients are derived via type classes
- Maximum compile-time verification
- Complex type errors
- No runtime route structure

## Proposed Design for Hermes

We propose a **hybrid approach** that combines the best of both:

1. **Type-level route specification** (like Servant) for compile-time safety
2. **Directive-style combinators** (like Akka HTTP) for ergonomic composition
3. **First-class integration** with Hermes's existing type-safe primitives

### Design Philosophy

- Leverage Haskell's type system without requiring PhD-level type-fu
- Produce readable error messages
- Support both "type-first" and "handler-first" development styles
- Maintain Hermes's performance focus with zero-cost abstractions where possible

---

## Part 1: Core Route Type

### 1.1 The Route Monad

```haskell
-- | The core routing monad, parameterized by:
--   * 'e' - Error type for route failures
--   * 'm' - Base monad (typically IO or some effect monad)
--   * 'a' - Result type
newtype RouteT e m a = RouteT
  { unRouteT :: RequestContext -> m (RouteResult e a)
  }
  deriving (Functor)

-- | Result of attempting to match a route
data RouteResult e a
  = Matched !a                    -- ^ Successfully matched and produced a value
  | Rejected !(Rejection e)       -- ^ Did not match, try alternatives
  | Failed !e                     -- ^ Matched but failed (don't try alternatives)
  deriving (Functor, Show)

-- | Rejection information for debugging and error responses
data Rejection e
  = MethodRejection !Method [Method]       -- ^ Wrong method, expected one of these
  | PathRejection                          -- ^ Path didn't match
  | HeaderRejection !HeaderFieldName !Text -- ^ Required header missing or invalid
  | ContentTypeRejection !MediaType        -- ^ Content type not acceptable
  | QueryParamRejection !Text !Text        -- ^ Query parameter issue
  | AuthenticationRejection !Text          -- ^ Authentication failed
  | CustomRejection !e                     -- ^ Application-specific rejection
  deriving (Show, Eq)

-- | Request context passed through the routing tree
data RequestContext = RequestContext
  { rcRequest :: !Request         -- ^ The WAI Request
  , rcUnmatchedPath :: ![Text]    -- ^ Path segments not yet matched
  , rcHeaders :: !HeaderMap       -- ^ Parsed headers (from Hermes)
  , rcSettings :: !RouteSettings  -- ^ Configuration
  }
```

### 1.2 Route Combinators

```haskell
-- | Try the first route; if it rejects, try the second
(<|>) :: RouteT e m a -> RouteT e m a -> RouteT e m a

-- | Sequential composition - first must match, then second
(</>) :: RouteT e m a -> (a -> RouteT e m b) -> RouteT e m b

-- | Map over the result
(<$>) :: (a -> b) -> RouteT e m a -> RouteT e m b

-- | Apply a function in a route context
(<*>) :: RouteT e m (a -> b) -> RouteT e m a -> RouteT e m b
```

---

## Part 2: Path Matching Directives

### 2.1 Static Path Segments

```haskell
-- | Match a literal path segment
path :: Text -> RouteT e m ()
path segment = RouteT $ \ctx ->
  case rcUnmatchedPath ctx of
    (s:rest) | s == segment ->
      pure $ Matched (ctx { rcUnmatchedPath = rest }, ())
    _ -> pure $ Rejected PathRejection

-- | Match a path prefix (multiple segments)
pathPrefix :: [Text] -> RouteT e m ()
pathPrefix segments = traverse_ path segments

-- | Ensure the path is fully consumed
pathEnd :: RouteT e m ()
pathEnd = RouteT $ \ctx ->
  case rcUnmatchedPath ctx of
    [] -> pure $ Matched ()
    _  -> pure $ Rejected PathRejection
```

### 2.2 Path Extraction (Captures)

```haskell
-- | Type class for values that can be extracted from path segments
class PathPiece a where
  parsePathPiece :: Text -> Maybe a
  renderPathPiece :: a -> Text

instance PathPiece Int where
  parsePathPiece = readMaybe . T.unpack
  renderPathPiece = T.pack . show

instance PathPiece Text where
  parsePathPiece = Just
  renderPathPiece = id

instance PathPiece UUID where
  parsePathPiece = UUID.fromText
  renderPathPiece = UUID.toText

-- | Capture a path segment and parse it
capture :: forall a e m. PathPiece a => RouteT e m a
capture = RouteT $ \ctx ->
  case rcUnmatchedPath ctx of
    (s:rest) -> case parsePathPiece @a s of
      Just a  -> pure $ Matched a
      Nothing -> pure $ Rejected PathRejection
    [] -> pure $ Rejected PathRejection

-- | Capture all remaining path segments
captureAll :: RouteT e m [Text]
captureAll = RouteT $ \ctx ->
  pure $ Matched (rcUnmatchedPath ctx)
```

### 2.3 Path Matchers DSL

```haskell
-- | Type-safe path matcher combining static and dynamic segments
data PathMatcher a where
  Static   :: Text -> PathMatcher ()
  Capture  :: PathPiece a => PathMatcher a
  (:/)     :: PathMatcher a -> PathMatcher b -> PathMatcher (a, b)
  End      :: PathMatcher ()

-- | Smart constructor for cleaner syntax
(/>) :: PathMatcher a -> PathMatcher b -> PathMatcher (a, b)
(/>) = (:/)

-- | Match a complete path pattern
matchPath :: PathMatcher a -> RouteT e m a
matchPath = \case
  Static t     -> path t $> ()
  Capture      -> capture
  End          -> pathEnd
  (ma :/  mb)  -> (,) <$> matchPath ma <*> matchPath mb
```

**Example usage:**

```haskell
-- Match: /users/{id}/posts/{postId}
userPostPath :: PathMatcher (Int, Int)
userPostPath = Static "users" /> Capture /> Static "posts" /> Capture /> End

route :: RouteT e m Response
route = do
  (userId, postId) <- matchPath userPostPath
  -- userId and postId are both Int, type-checked!
  getUserPost userId postId
```

---

## Part 3: Method Directives

### 3.1 Method Matching

```haskell
-- | Match a specific HTTP method
method :: Method -> RouteT e m ()
method expected = RouteT $ \ctx ->
  let actual = requestMethod (rcRequest ctx)
  in if actual == expected
     then pure $ Matched ()
     else pure $ Rejected $ MethodRejection actual [expected]

-- | Convenience methods
get, post, put, delete, patch, head, options :: RouteT e m ()
get     = method mGet
post    = method mPost
put     = method mPut
delete  = method mDelete
patch   = method mPatch
head    = method mHead
options = method mOptions

-- | Match any of the given methods
methods :: [Method] -> RouteT e m Method
methods allowed = RouteT $ \ctx ->
  let actual = requestMethod (rcRequest ctx)
  in if actual `elem` allowed
     then pure $ Matched actual
     else pure $ Rejected $ MethodRejection actual allowed
```

---

## Part 4: Header Directives

Integrating with Hermes's existing `KnownHeader` infrastructure:

### 4.1 Header Extraction

```haskell
-- | Extract a required header (rejects if missing or parse fails)
header :: forall h e m.
  ( KnownHeader h
  , Direction h `AllowedIn` 'Request
  ) => RouteT e m h
header = RouteT $ \ctx ->
  case lookupHeader @h (rcHeaders ctx) of
    Right (Just h) -> pure $ Matched h
    Right Nothing  -> pure $ Rejected $
      HeaderRejection (headerName (Proxy @h)) "missing"
    Left err       -> pure $ Rejected $
      HeaderRejection (headerName (Proxy @h)) (T.pack $ show err)

-- | Extract an optional header
optionalHeader :: forall h e m.
  ( KnownHeader h
  , Direction h `AllowedIn` 'Request
  ) => RouteT e m (Maybe h)
optionalHeader = RouteT $ \ctx ->
  case lookupHeader @h (rcHeaders ctx) of
    Right mh   -> pure $ Matched mh
    Left err   -> pure $ Rejected $
      HeaderRejection (headerName (Proxy @h)) (T.pack $ show err)

-- | Type family to verify header direction
type family AllowedIn (dir :: HeaderIsRequestOrResponse) (ctx :: HeaderIsRequestOrResponse) :: Constraint where
  AllowedIn 'Request 'Request           = ()
  AllowedIn 'Response 'Response         = ()
  AllowedIn 'RequestAndResponse _       = ()
  AllowedIn _ _                          = TypeError
    ('Text "Header direction mismatch")
```

### 4.2 Content Negotiation

```haskell
-- | Extract Accept header and negotiate content type
acceptsContentType :: RouteT e m MediaType
acceptsContentType = do
  accept <- optionalHeader @Accept
  -- Return negotiated media type based on what server can produce
  pure $ negotiateMediaType accept serverCapabilities

-- | Require a specific content type in the request
requireContentType :: MediaType -> RouteT e m ()
requireContentType expected = do
  actual <- header @ContentType
  when (contentTypeMedia actual /= expected) $
    reject $ ContentTypeRejection expected
```

---

## Part 5: Body Handling

### 5.1 Request Body Extraction

```haskell
-- | Type class for deserializing request bodies
class FromRequestBody a where
  type BodyContentType a :: MediaType
  parseRequestBody :: ByteString -> Either Text a

-- | Extract and parse the request body
body :: forall a e m. FromRequestBody a => RouteT e m a
body = do
  requireContentType (bodyContentType @a)
  bs <- requestBody
  case parseRequestBody @a bs of
    Right a  -> pure a
    Left err -> fail $ "Body parse error: " <> err

-- | JSON body parsing (uses Aeson)
instance FromJSON a => FromRequestBody (Json a) where
  type BodyContentType (Json a) = "application/json"
  parseRequestBody = first T.pack . eitherDecodeStrict . unJson

-- | Form body parsing
instance FromForm a => FromRequestBody (Form a) where
  type BodyContentType (Form a) = "application/x-www-form-urlencoded"
  parseRequestBody = parseForm
```

### 5.2 Response Body Rendering

```haskell
-- | Type class for serializing response bodies
class ToResponseBody a where
  responseContentType :: MediaType
  renderResponseBody :: a -> Builder

-- | Complete the route with a response
complete :: ToResponseBody a => StatusCode -> a -> RouteT e m Response
complete status body = pure $ Response
  { responseStatus = status
  , responseHeaders = [(hContentType, renderMediaType $ responseContentType @a)]
  , responseBody = renderResponseBody body
  }

-- | Common completion helpers
ok :: ToResponseBody a => a -> RouteT e m Response
ok = complete status200

created :: ToResponseBody a => a -> RouteT e m Response
created = complete status201

noContent :: RouteT e m Response
noContent = pure $ Response status204 [] mempty
```

---

## Part 6: Query Parameters

### 6.1 Query Parameter Extraction

```haskell
-- | Type class for query parameter parsing
class FromQueryParam a where
  parseQueryParam :: Maybe Text -> Either Text a

instance FromQueryParam Text where
  parseQueryParam = maybe (Left "missing") Right

instance FromQueryParam Int where
  parseQueryParam Nothing = Left "missing"
  parseQueryParam (Just t) = maybe (Left "not an integer") Right $ readMaybe $ T.unpack t

instance FromQueryParam a => FromQueryParam (Maybe a) where
  parseQueryParam Nothing = Right Nothing
  parseQueryParam (Just t) = Just <$> parseQueryParam (Just t)

-- | Extract a required query parameter
queryParam :: forall a e m. FromQueryParam a => Text -> RouteT e m a
queryParam name = RouteT $ \ctx ->
  let params = queryString (rcRequest ctx)
      value = join $ lookup (encodeUtf8 name) params
  in case parseQueryParam @a (decodeUtf8 <$> value) of
    Right a  -> pure $ Matched a
    Left err -> pure $ Rejected $ QueryParamRejection name err

-- | Extract an optional query parameter
optionalQueryParam :: forall a e m. FromQueryParam a => Text -> RouteT e m (Maybe a)
optionalQueryParam name = RouteT $ \ctx ->
  let params = queryString (rcRequest ctx)
      value = join $ lookup (encodeUtf8 name) params
  in case value of
    Nothing -> pure $ Matched Nothing
    Just v  -> case parseQueryParam @a (Just $ decodeUtf8 v) of
      Right a  -> pure $ Matched (Just a)
      Left err -> pure $ Rejected $ QueryParamRejection name err

-- | Extract multiple values for a query parameter
queryParams :: forall a e m. FromQueryParam a => Text -> RouteT e m [a]
queryParams name = RouteT $ \ctx ->
  let params = queryString (rcRequest ctx)
      values = [ v | (k, Just v) <- params, k == encodeUtf8 name ]
  in case traverse (parseQueryParam @a . Just . decodeUtf8) values of
    Right as -> pure $ Matched as
    Left err -> pure $ Rejected $ QueryParamRejection name err
```

---

## Part 7: Type-Level API Specification (Servant-style)

For users who prefer the Servant approach, we provide a type-level DSL:

### 7.1 API Combinators

```haskell
-- | Path segment
data (path :: Symbol) :> (api :: Type)
infixr 9 :>

-- | Alternative routes
data (a :: Type) :<|> (b :: Type) = a :<|> b
infixr 8 :<|>

-- | Capture a path segment
data Capture (name :: Symbol) (a :: Type)

-- | Query parameter
data QueryParam (name :: Symbol) (a :: Type)

-- | Request body
data ReqBody (contentTypes :: [Type]) (a :: Type)

-- | HTTP Methods with response types
data Get (contentTypes :: [Type]) (a :: Type)
data Post (contentTypes :: [Type]) (a :: Type)
data Put (contentTypes :: [Type]) (a :: Type)
data Delete (contentTypes :: [Type]) (a :: Type)
data Patch (contentTypes :: [Type]) (a :: Type)

-- | Header requirement
data Header (name :: Symbol) (a :: Type)
```

### 7.2 Example API

```haskell
type UserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" UserId :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
  :<|> "users" :> Capture "id" UserId :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
  :<|> "users" :> Capture "id" UserId :> Delete '[JSON] NoContent
```

### 7.3 Server Derivation

```haskell
-- | Type family to compute the server handler type
type family ServerT (api :: Type) (m :: Type -> Type) :: Type where
  ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m
  ServerT (path :> api) m = ServerT api m
  ServerT (Capture name a :> api) m = a -> ServerT api m
  ServerT (QueryParam name a :> api) m = Maybe a -> ServerT api m
  ServerT (ReqBody cts a :> api) m = a -> ServerT api m
  ServerT (Header name a :> api) m = a -> ServerT api m
  ServerT (Get cts a) m = m a
  ServerT (Post cts a) m = m a
  ServerT (Put cts a) m = m a
  ServerT (Delete cts a) m = m a
  ServerT (Patch cts a) m = m a

-- | Class to convert API type + handlers to routes
class HasServer api where
  route :: Proxy api -> ServerT api Handler -> RouteT ServerError IO Response

-- Example server implementation
userServer :: ServerT UserAPI Handler
userServer =
       listUsers
  :<|> getUser
  :<|> createUser
  :<|> updateUser
  :<|> deleteUser
  where
    listUsers :: Handler [User]
    listUsers = query "SELECT * FROM users"

    getUser :: UserId -> Handler User
    getUser uid = queryOne "SELECT * FROM users WHERE id = ?" uid

    createUser :: NewUser -> Handler User
    createUser = insert "users"

    updateUser :: UserId -> UpdateUser -> Handler User
    updateUser uid upd = update "users" uid upd

    deleteUser :: UserId -> Handler NoContent
    deleteUser uid = delete "users" uid $> NoContent
```

### 7.4 Client Derivation

```haskell
-- | Type family to compute client function types
type family ClientT (api :: Type) (m :: Type -> Type) :: Type where
  ClientT (a :<|> b) m = ClientT a m :<|> ClientT b m
  ClientT (path :> api) m = ClientT api m
  ClientT (Capture name a :> api) m = a -> ClientT api m
  ClientT (QueryParam name a :> api) m = Maybe a -> ClientT api m
  ClientT (ReqBody cts a :> api) m = a -> ClientT api m
  ClientT (Get cts a) m = m a
  ClientT (Post cts a) m = m a
  -- ... etc

-- | Class to generate client functions
class HasClient api where
  clientWithRoute :: Proxy api -> ClientEnv -> ClientT api ClientM

-- | Generate client functions
client :: HasClient api => Proxy api -> ClientT api ClientM
client = clientWithRoute Proxy defaultClientEnv

-- Example usage
listUsers :: ClientM [User]
getUser :: UserId -> ClientM User
createUser :: NewUser -> ClientM User
(listUsers :<|> getUser :<|> createUser :<|> _ :<|> _) = client (Proxy @UserAPI)
```

---

## Part 8: Authentication & Authorization

### 8.1 Authentication Directives

```haskell
-- | Authentication result
data AuthResult a
  = Authenticated a
  | AuthFailed Text
  | NoCredentials

-- | Type class for authentication schemes
class AuthScheme scheme where
  type AuthData scheme :: Type
  authenticate :: scheme -> RouteT e m (AuthResult (AuthData scheme))

-- | Bearer token authentication
data BearerAuth = BearerAuth
  { validateToken :: Text -> IO (Maybe User)
  }

instance AuthScheme BearerAuth where
  type AuthData BearerAuth = User
  authenticate scheme = do
    auth <- optionalHeader @Authorization
    case auth of
      Nothing -> pure NoCredentials
      Just (Bearer token) -> liftIO $ do
        mUser <- validateToken scheme token
        pure $ maybe (AuthFailed "Invalid token") Authenticated mUser
      Just _ -> pure $ AuthFailed "Expected Bearer authentication"

-- | Require authentication
authenticated :: AuthScheme scheme => scheme -> RouteT e m (AuthData scheme)
authenticated scheme = do
  result <- authenticate scheme
  case result of
    Authenticated user -> pure user
    AuthFailed msg     -> fail $ AuthenticationRejection msg
    NoCredentials      -> fail $ AuthenticationRejection "No credentials provided"

-- | Optional authentication
optionalAuth :: AuthScheme scheme => scheme -> RouteT e m (Maybe (AuthData scheme))
optionalAuth scheme = do
  result <- authenticate scheme
  pure $ case result of
    Authenticated user -> Just user
    _                  -> Nothing
```

### 8.2 Authorization

```haskell
-- | Permission type class
class HasPermission user permission where
  hasPermission :: user -> permission -> Bool

-- | Require a specific permission
authorize :: HasPermission user perm => user -> perm -> RouteT e m ()
authorize user perm =
  unless (hasPermission user perm) $
    fail $ AuthorizationRejection "Insufficient permissions"

-- | Combined auth + authz directive
withAuth :: (AuthScheme scheme, HasPermission (AuthData scheme) perm)
         => scheme -> perm -> RouteT e m (AuthData scheme)
withAuth scheme perm = do
  user <- authenticated scheme
  authorize user perm
  pure user
```

---

## Part 9: Middleware & Filters

### 9.1 Route Transformers

```haskell
-- | Transform a route (middleware)
type Middleware = forall e m a. RouteT e m a -> RouteT e m a

-- | Log all requests
loggingMiddleware :: Middleware
loggingMiddleware route = RouteT $ \ctx -> do
  let req = rcRequest ctx
  liftIO $ logInfo $ "Request: " <> show (requestMethod req) <> " " <> show (rawPathInfo req)
  result <- unRouteT route ctx
  liftIO $ case result of
    Matched _ -> logInfo "Response: matched"
    Rejected r -> logInfo $ "Response: rejected - " <> show r
    Failed e -> logInfo $ "Response: failed - " <> show e
  pure result

-- | Add CORS headers
corsMiddleware :: CorsConfig -> Middleware
corsMiddleware config route = RouteT $ \ctx ->
  if requestMethod (rcRequest ctx) == mOptions
  then pure $ Matched $ corsPreflightResponse config
  else do
    result <- unRouteT route ctx
    pure $ addCorsHeaders config <$> result

-- | Rate limiting
rateLimitMiddleware :: RateLimiter -> Middleware
rateLimitMiddleware limiter route = RouteT $ \ctx -> do
  allowed <- liftIO $ checkRateLimit limiter (remoteHost $ rcRequest ctx)
  if allowed
    then unRouteT route ctx
    else pure $ Failed TooManyRequests

-- | Compose middleware
(>>>) :: Middleware -> Middleware -> Middleware
(f >>> g) route = f (g route)
```

### 9.2 Exception Handling

```haskell
-- | Catch exceptions and convert to route failures
catchRoute :: Exception ex => (ex -> RouteT e m a) -> RouteT e m a -> RouteT e m a
catchRoute handler route = RouteT $ \ctx -> do
  result <- try (unRouteT route ctx)
  case result of
    Left ex     -> unRouteT (handler ex) ctx
    Right res   -> pure res

-- | Handle specific error types
handleErrors :: (e -> Response) -> RouteT e m Response -> RouteT () m Response
handleErrors handler route = RouteT $ \ctx -> do
  result <- unRouteT route ctx
  pure $ case result of
    Matched resp -> Matched resp
    Rejected r   -> Rejected r
    Failed e     -> Matched (handler e)
```

---

## Part 10: Running Routes

### 10.1 WAI Integration

```haskell
-- | Convert a route to a WAI Application
toApplication :: RouteT ServerError IO Response -> Application
toApplication route request respond = do
  let ctx = RequestContext
        { rcRequest = request
        , rcUnmatchedPath = pathInfo request
        , rcHeaders = headerMapFromList (requestHeaders request)
        , rcSettings = defaultRouteSettings
        }
  result <- runRouteT route ctx
  respond $ case result of
    Matched resp -> toWaiResponse resp
    Rejected rej -> rejectionResponse rej
    Failed err   -> errorResponse err

-- | Run with the Hermes SimpleServer
runHermesServer :: ServerSettings -> RouteT ServerError IO Response -> IO ()
runHermesServer settings route =
  runWithPort settings (toApplication route)
```

### 10.2 Error Responses

```haskell
-- | Convert rejection to HTTP response
rejectionResponse :: Rejection e -> Wai.Response
rejectionResponse = \case
  MethodRejection actual allowed ->
    responseLBS status405
      [(hAllow, BS.intercalate ", " $ map (fromMethod . fromMethod) allowed)]
      "Method Not Allowed"
  PathRejection ->
    responseLBS status404 [] "Not Found"
  HeaderRejection name msg ->
    responseLBS status400 [] $ "Bad Request: header " <> name <> " - " <> msg
  ContentTypeRejection expected ->
    responseLBS status415 [] $ "Unsupported Media Type: expected " <> show expected
  AuthenticationRejection msg ->
    responseLBS status401 [(hWWWAuthenticate, "Bearer")] msg
  _ ->
    responseLBS status400 [] "Bad Request"
```

---

## Part 11: Testing Support

### 11.1 Route Testing DSL

```haskell
-- | Test context
data TestRequest = TestRequest
  { testMethod :: Method
  , testPath :: Text
  , testHeaders :: [(HeaderFieldName, ByteString)]
  , testBody :: ByteString
  , testQueryParams :: [(Text, Text)]
  }

-- | Create test requests
testGet :: Text -> TestRequest
testGet path = TestRequest mGet path [] "" []

testPost :: Text -> ByteString -> TestRequest
testPost path body = TestRequest mPost path [(hContentType, "application/json")] body []

-- | Run a route with a test request
runTestRoute :: RouteT e IO a -> TestRequest -> IO (RouteResult e a)
runTestRoute route req = do
  waiReq <- toWaiRequest req
  let ctx = testContext waiReq
  runRouteT route ctx

-- | Assertions
shouldMatch :: (Show a, Eq a) => RouteResult e a -> a -> Expectation
shouldMatch (Matched actual) expected = actual `shouldBe` expected
shouldMatch result _ = expectationFailure $ "Expected Matched, got: " <> show result

shouldReject :: RouteResult e a -> Expectation
shouldReject (Rejected _) = pure ()
shouldReject result = expectationFailure $ "Expected Rejected, got: " <> show result
```

### 11.2 Property-Based Testing

```haskell
-- | Generate valid paths for an API type
class Arbitrary (ValidPath api) => HasArbitraryPath api where
  type ValidPath api :: Type
  arbitraryPath :: Gen (ValidPath api, Text)

-- | Verify client/server round-trip
prop_clientServerRoundTrip :: forall api.
  (HasServer api, HasClient api, Eq (ResponseOf api)) =>
  ServerT api Handler -> Property
prop_clientServerRoundTrip server = property $ do
  (input, path) <- arbitraryPath @api
  let serverApp = toApplication (route (Proxy @api) server)
  response <- runClientWithApp serverApp (clientRequest @api input)
  response `shouldBe` serverResponse @api server input
```

---

## Part 12: Implementation Phases

### Phase 1: Core Route Monad (2-3 weeks effort)
- [ ] Implement `RouteT` monad
- [ ] Basic path matching (`path`, `pathEnd`, `capture`)
- [ ] Method matching
- [ ] Route composition (`<|>`, `</>`)
- [ ] WAI integration

### Phase 2: Header & Body Integration (2 weeks effort)
- [ ] Header directive integration with `KnownHeader`
- [ ] Content negotiation
- [ ] Body parsing/rendering type classes
- [ ] JSON, form, and multipart support

### Phase 3: Query Parameters & Auth (1-2 weeks effort)
- [ ] Query parameter extraction
- [ ] Authentication framework
- [ ] Authorization helpers

### Phase 4: Type-Level API (3-4 weeks effort)
- [ ] Type-level combinators
- [ ] Server derivation via type classes
- [ ] Client derivation
- [ ] OpenAPI generation

### Phase 5: Testing & Middleware (1-2 weeks effort)
- [ ] Testing DSL
- [ ] Common middleware (logging, CORS, rate limiting)
- [ ] Error handling refinement

### Phase 6: Documentation & Polish (1-2 weeks effort)
- [ ] Tutorial documentation
- [ ] API reference
- [ ] Example applications
- [ ] Performance benchmarks

---

## Part 13: Example Application

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hermes.Routing
import Hermes.Headers
import Hermes.Server

-- | Domain types
data User = User { userId :: Int, userName :: Text, userEmail :: Text }
  deriving (Generic, ToJSON, FromJSON)

data NewUser = NewUser { newUserName :: Text, newUserEmail :: Text }
  deriving (Generic, FromJSON)

-- | Type-level API
type API =
       "api" :> "v1" :> "users" :> Get '[JSON] [User]
  :<|> "api" :> "v1" :> "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "api" :> "v1" :> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
  :<|> "api" :> "v1" :> "health" :> Get '[PlainText] Text

-- | Route-style API (alternative)
routes :: RouteT ServerError IO Response
routes =
  pathPrefix ["api", "v1"] $
    usersRoutes <|> healthRoute
  where
    usersRoutes = path "users" $
      (get >> pathEnd >> listUsersHandler)
      <|> (get >> capture >>= getUserHandler)
      <|> (post >> pathEnd >> body @(Json NewUser) >>= createUserHandler)

    healthRoute = path "health" >> get >> pathEnd >> ok "OK"

-- | Handlers
listUsersHandler :: RouteT ServerError IO Response
listUsersHandler = do
  users <- liftIO $ Database.query "SELECT * FROM users"
  ok (Json users)

getUserHandler :: Int -> RouteT ServerError IO Response
getUserHandler uid = do
  mUser <- liftIO $ Database.queryOne "SELECT * FROM users WHERE id = ?" uid
  case mUser of
    Just user -> ok (Json user)
    Nothing   -> notFound "User not found"

createUserHandler :: Json NewUser -> RouteT ServerError IO Response
createUserHandler (Json newUser) = do
  user <- liftIO $ Database.insert "users" newUser
  created (Json user)

-- | Main
main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  runHermesServer defaultSettings { port = 8080 } routes
```

---

## Appendix A: Comparison Matrix

| Feature | Akka HTTP | Servant | Hermes (Proposed) |
|---------|-----------|---------|-------------------|
| Route Definition | Runtime DSL | Type-level | Both |
| Type Safety | Moderate | Maximum | High |
| Error Messages | Good | Complex | Good (goal) |
| Client Generation | Manual | Automatic | Automatic |
| Server Generation | Manual | Automatic | Automatic |
| OpenAPI/Swagger | Plugin | Built-in | Planned |
| Performance | Excellent | Excellent | Excellent (goal) |
| Learning Curve | Moderate | Steep | Moderate (goal) |
| Middleware | Directives | Combinator | Both |
| Header Type Safety | Limited | Good | Excellent (existing) |

---

## Appendix B: Key Dependencies

```yaml
dependencies:
  # Existing Hermes deps
  - wai >= 3.2.3
  - http-types
  - flatparse
  - mason
  - symbolize

  # New routing deps
  - aeson           # JSON support
  - http-media      # Content negotiation
  - http-api-data   # Path/query parsing
  - mtl             # Monad transformers
  - unliftio        # Async support
  - vault           # Request-local storage
```

---

## References

- [Akka HTTP Routing DSL](https://doc.akka.io/docs/akka-http/current/routing-dsl/overview.html)
- [Akka HTTP Directives](https://doc.akka.io/docs/akka-http/current/routing-dsl/directives/index.html)
- [Servant Documentation](https://www.servant.dev/)
- [Type-level Web APIs with Servant (Paper)](https://www.andres-loeh.de/Servant/servant-wgp.pdf)
- [WAI Interface](https://hackage.haskell.org/package/wai)
- [RFC 9110 - HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110)
