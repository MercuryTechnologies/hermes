# Type-Safe Routing Design for Hermes

## Executive Summary

This document specifies the design for expanding Hermes into a full type-safe HTTP framework comparable to [Akka HTTP](https://doc.akka.io/docs/akka-http/current/routing-dsl/overview.html) and [Servant](https://www.servant.dev/). The goal is to provide:

1. **Type-safe routing** - Compile-time verification that routes are well-formed
2. **Bidirectional derivation** - Generate both servers and clients from the same specification
3. **Composable directives** - Build complex routes from simple, reusable building blocks
4. **Integration with existing Hermes primitives** - Leverage the existing `KnownHeader`, `Method`, `StatusCode` infrastructure
5. **Template Haskell support** - Compile-time route generation with better error messages and zero runtime overhead
6. **Backend-agnostic design** - A new application interface ("HAI") that improves on WAI with more performant types, while supporting multiple backends

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

The routing monad is parameterized by a region type `r` that ensures request data
cannot escape its scope without explicit copying. This uses ST-style rank-2 types
for compile-time safety.

```haskell
{-# LANGUAGE RankNTypes #-}

-- | The core routing monad, parameterized by:
--   * 'r' - Region tag (prevents request data escape, like ST's 's')
--   * 'e' - Error type for route failures
--   * 'm' - Base monad (typically IO or some effect monad)
--   * 'a' - Result type
newtype RouteT r e m a = RouteT
  { unRouteT :: RequestContext r -> m (RouteResult e a)
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
-- The 'r' parameter tags all request data to this region
data RequestContext r = RequestContext
  { rcRequest :: !(Request r)     -- ^ Region-scoped request (from HAI)
  , rcUnmatchedPath :: ![Text]    -- ^ Path segments not yet matched (copied for routing)
  , rcHeaders :: !(HeaderMap r)   -- ^ Arena-allocated headers
  , rcArena :: !(Arena r)         -- ^ Arena for temporary allocations
  , rcSettings :: !RouteSettings  -- ^ Configuration
  }

-- | Run a route handler with a request-scoped arena
-- The rank-2 type ensures nothing tagged with 'r' can escape
runRouteT :: forall e m a. MonadIO m
          => RouteSettings
          -> (forall r. RouteT r e m a)  -- ^ Handler cannot leak 'r'
          -> RawRequest
          -> m (Either (Rejection e) a)
runRouteT settings handler rawReq = withRequestArena defaultArenaSize $ \arena -> do
  ctx <- parseRequestContext arena rawReq settings
  result <- unRouteT handler ctx
  case result of
    Matched a    -> pure (Right a)
    Rejected rej -> pure (Left rej)
    Failed e     -> pure (Left (CustomRejection e))
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

### 7.5 Record-Based Routes (NamedRoutes Style)

Inspired by [Servant's NamedRoutes](https://www.tweag.io/blog/2022-02-24-named-routes/),
Hermes supports a more ergonomic record-based API definition. This approach:
- Provides named field accessors for client functions (no pattern matching needed)
- Supports nested API composition via records
- Produces cleaner type errors
- Works with Generic deriving

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Mode type for API interpretation
data RouteMode = AsAPI | AsServer Type | AsClient Type

-- | Type family that interprets routes based on mode
type family (:-) (mode :: RouteMode) (api :: Type) :: Type where
  AsAPI       :- api = api
  AsServer m  :- api = ServerT api m
  AsClient m  :- api = ClientT api m

-- | Record-based API definition
data UserRoutes mode = UserRoutes
  { _listUsers  :: mode :- Get '[JSON] [User]
  , _getUser    :: mode :- Capture "id" UserId :> Get '[JSON] User
  , _createUser :: mode :- ReqBody '[JSON] NewUser :> Post '[JSON] User
  , _updateUser :: mode :- Capture "id" UserId :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
  , _deleteUser :: mode :- Capture "id" UserId :> Delete '[JSON] NoContent
  } deriving (Generic)

-- | Nested API with multiple resources
data APIRoutes mode = APIRoutes
  { _users    :: mode :- "users" :> NamedRoutes UserRoutes
  , _posts    :: mode :- "posts" :> NamedRoutes PostRoutes
  , _health   :: mode :- "health" :> Get '[PlainText] Text
  , _metrics  :: mode :- "metrics" :> Get '[PlainText] Text
  } deriving (Generic)

-- | Top-level API type
type API = "api" :> "v1" :> NamedRoutes APIRoutes
```

#### Server Implementation with Records

```haskell
-- | Server handlers as a record - much cleaner than :<|> chains!
userHandlers :: UserRoutes (AsServer Handler)
userHandlers = UserRoutes
  { _listUsers  = Database.getAllUsers
  , _getUser    = Database.getUser
  , _createUser = Database.createUser
  , _updateUser = Database.updateUser
  , _deleteUser = Database.deleteUser
  }

apiHandlers :: APIRoutes (AsServer Handler)
apiHandlers = APIRoutes
  { _users   = userHandlers
  , _posts   = postHandlers
  , _health  = pure "OK"
  , _metrics = getMetrics
  }

-- | Convert record to route
server :: RouteT ServerError Handler Response
server = genericServer (Proxy @API) apiHandlers
```

#### Client Functions with Records

```haskell
-- | Client as a record - named fields, no pattern matching!
apiClient :: APIRoutes (AsClient ClientM)
apiClient = genericClient (Proxy @API)

-- | Use client functions directly by field name
example :: ClientM [User]
example = do
  -- Named access - much clearer than positional!
  users <- _listUsers (_users apiClient)

  -- Nested access
  user <- _getUser (_users apiClient) (UserId 42)

  -- Health check
  _ <- _health apiClient

  pure users
```

#### Generic Derivation

```haskell
-- | Type class for generic route derivation
class GServantProduct f where
  type GToServant f :: Type
  gToServant :: f p -> GToServant f
  gFromServant :: GToServant f -> f p

-- | Convert record to :<|> tree
class GenericRoutes routes where
  type ToServantApi routes :: Type
  genericApi :: routes AsAPI -> ToServantApi routes

-- | Derive server from record
genericServer :: forall api routes m.
  ( GenericRoutes routes
  , HasServer (ToServantApi routes)
  ) => Proxy api -> routes (AsServer m) -> ServerT (ToServantApi routes) m

-- | Derive client from record
genericClient :: forall api routes m.
  ( GenericRoutes routes
  , HasClient (ToServantApi routes)
  ) => Proxy api -> routes (AsClient m)
```

#### Nested Routes with Authentication

```haskell
-- | Public routes (no auth required)
data PublicRoutes mode = PublicRoutes
  { _login    :: mode :- "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Token
  , _register :: mode :- "register" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
  , _health   :: mode :- "health" :> Get '[PlainText] Text
  } deriving (Generic)

-- | Protected routes (auth required)
data ProtectedRoutes mode = ProtectedRoutes
  { _profile  :: mode :- "profile" :> Get '[JSON] User
  , _settings :: mode :- "settings" :> NamedRoutes SettingsRoutes
  , _admin    :: mode :- "admin" :> NamedRoutes AdminRoutes
  } deriving (Generic)

-- | Combined API with auth boundary
data AppRoutes mode = AppRoutes
  { _public    :: mode :- "public" :> NamedRoutes PublicRoutes
  , _protected :: mode :- "protected" :> AuthProtect "jwt" :> NamedRoutes ProtectedRoutes
  } deriving (Generic)

-- | Server with auth handling
appServer :: AppRoutes (AsServer Handler)
appServer = AppRoutes
  { _public = publicHandlers
  , _protected = \authUser -> protectedHandlers authUser  -- Auth user passed in!
  }
```

---

## Part 8: Template Haskell Support

Template Haskell provides compile-time metaprogramming for route generation, validation, and optimization. This is a key differentiator from both Servant (which relies purely on type-level programming with often inscrutable errors) and Akka HTTP (which has no compile-time route analysis).

### 8.1 Route Quasi-Quoters

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Quasi-quoter for defining routes with compile-time validation
-- Syntax: METHOD /path/segments/:capture -> handlerName
[routes|
  GET    /users                     -> listUsers
  GET    /users/:userId             -> getUser
  POST   /users                     -> createUser
  PUT    /users/:userId             -> updateUser
  DELETE /users/:userId             -> deleteUser
  GET    /users/:userId/posts       -> getUserPosts
  GET    /users/:userId/posts/:postId -> getUserPost
|]

-- The above generates:
-- 1. A routing function that pattern-matches efficiently
-- 2. Type signatures that enforce handler types
-- 3. Compile-time validation of route conflicts
-- 4. Reverse routing functions for URL generation
```

### 8.2 Compile-Time Route Validation

```haskell
-- | TH function to validate routes at compile time
validateRoutes :: Q [Dec] -> Q [Dec]
validateRoutes routesQ = do
  routes <- routesQ
  -- Check for:
  -- 1. Ambiguous routes (two routes that could match the same path)
  -- 2. Unreachable routes (a more specific route after a general one)
  -- 3. Type mismatches between captures and handlers
  -- 4. Missing handlers
  checkAmbiguousRoutes routes
  checkUnreachableRoutes routes
  checkHandlerTypes routes
  pure routes

-- Example: This would fail at compile time
[routes|
  GET /users/:id     -> getUser      -- captures 'id' as Text
  GET /users/:userId -> getUserById  -- COMPILE ERROR: ambiguous with above
|]

-- Example: Unreachable route detection
[routes|
  GET /users/*       -> catchAll     -- catches everything
  GET /users/:id     -> getUser      -- COMPILE ERROR: unreachable
|]
```

### 8.3 Type-Safe Captures with TH

```haskell
-- | Generate capture parsers with custom types at compile time
mkCapture :: String -> TypeQ -> Q [Dec]
mkCapture name typ = [d|
  $(varP (mkName name)) :: PathPiece $(typ) => RouteT e m $(typ)
  $(varP (mkName name)) = capture @($(typ))
  |]

-- Usage:
$(mkCapture "userId" [t|UserId|])
$(mkCapture "postId" [t|PostId|])

-- Or with the routes QQ, specify types inline:
[routes|
  GET /users/:userId<UserId>/posts/:postId<PostId> -> getUserPost
|]
-- Generates: getUserPost :: UserId -> PostId -> Handler Response
```

### 8.4 Compile-Time Route Optimization

```haskell
-- | Generate an optimized routing trie at compile time
-- This eliminates runtime route parsing overhead
mkRoutingTrie :: [RouteSpec] -> Q Exp
mkRoutingTrie specs = do
  let trie = buildTrie specs
  -- Generate pattern matching code that mirrors the trie structure
  -- This compiles to efficient nested case expressions
  generateTrieMatching trie

-- The generated code looks like:
-- case segment1 of
--   "users" -> case segment2 of
--     [] -> usersHandler
--     (seg:rest) -> case parsePathPiece seg of
--       Just userId -> case rest of
--         [] -> getUserHandler userId
--         ["posts"] -> getUserPostsHandler userId
--         ...
--   "health" -> healthHandler
--   _ -> notFoundHandler
```

### 8.5 Reverse Routing (URL Generation)

```haskell
-- | Type-safe URL generation from route definitions
-- Generated automatically from route quasi-quoter

-- For route: GET /users/:userId/posts/:postId -> getUserPost
-- Generates:
getUserPostUrl :: UserId -> PostId -> Text
getUserPostUrl userId postId =
  "/users/" <> renderPathPiece userId <> "/posts/" <> renderPathPiece postId

-- With query parameters:
-- GET /search?q=:query&limit=:limit -> searchHandler
searchUrl :: Text -> Maybe Int -> Text
searchUrl query mLimit =
  "/search?q=" <> urlEncode query <> maybe "" (("&limit=" <>) . T.pack . show) mLimit

-- Link generation for type-level APIs
class HasLink api where
  type MkLink api :: Type
  toLink :: Proxy api -> MkLink api

-- Usage:
link :: Text
link = toLink (Proxy @("users" :> Capture "id" Int :> Get '[JSON] User)) 42
-- Result: "/users/42"
```

### 8.6 TH-Generated Handler Type Enforcement

```haskell
-- | Derive handler type signatures from route specifications
mkHandlers :: QuasiQuoter
mkHandlers = QuasiQuoter { quoteExp = parseHandlers }

-- Usage:
[mkHandlers|
  listUsers   : GET  /users           -> [User]
  getUser     : GET  /users/:Int      -> User
  createUser  : POST /users           <- NewUser -> User
  updateUser  : PUT  /users/:Int      <- UpdateUser -> User
  deleteUser  : DELETE /users/:Int    -> NoContent
|]

-- Generates type signatures:
-- listUsers  :: Handler [User]
-- getUser    :: Int -> Handler User
-- createUser :: NewUser -> Handler User
-- updateUser :: Int -> UpdateUser -> Handler User
-- deleteUser :: Int -> Handler NoContent

-- And stub implementations that fail at runtime if not implemented:
-- listUsers = notImplemented "listUsers"
-- etc.
```

### 8.7 Route Documentation Generation

```haskell
-- | Generate documentation at compile time
mkRouteDocs :: Q [Dec] -> Q Exp
mkRouteDocs routesQ = do
  routes <- analyzeRoutes routesQ
  -- Generate a documentation data structure
  [e| RouteDocumentation
        { docRoutes = $(listE $ map routeToDoc routes)
        , docVersion = $(stringE =<< runIO getPackageVersion)
        , docGenerated = $(stringE =<< runIO getCurrentTime)
        }
    |]

-- Can be used to generate:
-- - OpenAPI/Swagger specs at compile time
-- - Static documentation pages
-- - Client SDK documentation
```

### 8.8 Error Message Improvement

```haskell
-- | Custom type errors with helpful messages
type family ValidateRoute (route :: Type) :: Constraint where
  ValidateRoute (Capture name a :> Capture name' a' :> rest) =
    TypeError ('Text "Adjacent captures are ambiguous: /"
               ':<>: 'Text name ':<>: 'Text "/:" ':<>: 'Text name'
               ':$$: 'Text "Consider adding a static segment between them")
  ValidateRoute (path :> rest) = ValidateRoute rest
  ValidateRoute terminal = ()

-- TH can provide even better errors:
validateRoutesTH :: [RouteSpec] -> Q ()
validateRoutesTH routes = do
  forM_ (findConflicts routes) $ \(r1, r2) ->
    reportError $ unlines
      [ "Route conflict detected:"
      , "  Route 1: " ++ showRoute r1
      , "  Route 2: " ++ showRoute r2
      , ""
      , "These routes would match the same request."
      , "Consider making one more specific or combining them."
      ]
```

### 8.9 Runtime Route Overlap Detection (Non-TH)

For cases where TH isn't desired or possible, Hermes provides runtime route analysis
that can be run at application startup. This ensures manually constructed routes
also benefit from conflict detection.

```haskell
-- | Route specification for analysis
data RouteSpec = RouteSpec
  { routeMethod   :: !Method
  , routePattern  :: !PathPattern
  , routeHandler  :: !Text  -- Handler name for error messages
  }

-- | Path pattern for matching analysis
data PathPattern
  = StaticSegment !Text PathPattern
  | CaptureSegment !TypeRep PathPattern  -- TypeRep for type info
  | WildcardSegment                      -- Matches rest of path
  | EndOfPath
  deriving (Eq, Show)

-- | Result of route analysis
data RouteAnalysis = RouteAnalysis
  { analysisConflicts    :: ![(RouteSpec, RouteSpec, ConflictType)]
  , analysisUnreachable  :: ![RouteSpec]
  , analysisShadowed     :: ![(RouteSpec, RouteSpec)]  -- (shadowed, by)
  , analysisWarnings     :: ![Text]
  }

data ConflictType
  = Ambiguous        -- ^ Two routes could match the same request
  | MethodOverlap    -- ^ Same path, overlapping methods
  | CaptureConflict  -- ^ Different capture types at same position
  deriving (Show, Eq)

-- | Analyze routes for conflicts (pure function, no TH required)
analyzeRoutes :: [RouteSpec] -> RouteAnalysis
analyzeRoutes routes = RouteAnalysis
  { analysisConflicts   = findConflicts routes
  , analysisUnreachable = findUnreachable routes
  , analysisShadowed    = findShadowed routes
  , analysisWarnings    = generateWarnings routes
  }

-- | Check if two path patterns could match the same path
couldOverlap :: PathPattern -> PathPattern -> Bool
couldOverlap EndOfPath EndOfPath = True
couldOverlap WildcardSegment _ = True
couldOverlap _ WildcardSegment = True
couldOverlap (StaticSegment a rest1) (StaticSegment b rest2)
  | a == b    = couldOverlap rest1 rest2
  | otherwise = False
couldOverlap (CaptureSegment _ rest1) (CaptureSegment _ rest2) =
  couldOverlap rest1 rest2
couldOverlap (CaptureSegment _ rest1) (StaticSegment _ rest2) =
  couldOverlap rest1 rest2  -- Capture matches static
couldOverlap (StaticSegment _ rest1) (CaptureSegment _ rest2) =
  couldOverlap rest1 rest2  -- Static matches capture
couldOverlap _ _ = False

-- | Validate routes at startup, failing fast on conflicts
validateRoutesOrFail :: MonadIO m => [RouteSpec] -> m ()
validateRoutesOrFail routes = do
  let analysis = analyzeRoutes routes
  unless (null $ analysisConflicts analysis) $ liftIO $ do
    forM_ (analysisConflicts analysis) $ \(r1, r2, conflictType) ->
      hPutStrLn stderr $ unlines
        [ "ERROR: Route conflict (" <> show conflictType <> "):"
        , "  " <> show (routeMethod r1) <> " " <> showPattern (routePattern r1)
        , "    -> " <> T.unpack (routeHandler r1)
        , "  " <> show (routeMethod r2) <> " " <> showPattern (routePattern r2)
        , "    -> " <> T.unpack (routeHandler r2)
        ]
    exitFailure

-- | Extract route specs from a RouteT for analysis
class HasRouteSpecs route where
  extractRouteSpecs :: route -> [RouteSpec]

-- | Automatically extract specs from combined routes
instance (HasRouteSpecs a, HasRouteSpecs b) => HasRouteSpecs (a :<|> b) where
  extractRouteSpecs (a :<|> b) = extractRouteSpecs a <> extractRouteSpecs b

-- | Validate at server startup
runServerWithValidation :: Backend b
                        => BackendConfig b
                        -> RouteT ServerError IO Response
                        -> IO ()
runServerWithValidation config route = do
  validateRoutesOrFail (extractRouteSpecs route)
  runServer config route

-- | Development mode: log warnings but don't fail
validateRoutesWithWarnings :: MonadIO m => [RouteSpec] -> m ()
validateRoutesWithWarnings routes = do
  let analysis = analyzeRoutes routes
  forM_ (analysisWarnings analysis) $ \warning ->
    liftIO $ hPutStrLn stderr $ "WARNING: " <> T.unpack warning
  forM_ (analysisShadowed analysis) $ \(shadowed, by) ->
    liftIO $ hPutStrLn stderr $ unlines
      [ "WARNING: Route may be shadowed:"
      , "  " <> showRoute shadowed
      , "  is shadowed by:"
      , "  " <> showRoute by
      ]
```

### 8.10 Route Trie for Efficient Matching

The route analysis builds a trie structure that can also be used for efficient
runtime matching (when TH optimization isn't available):

```haskell
-- | Route trie for O(path length) matching
data RouteTrie a = RouteTrie
  { trieHandlers  :: !(HashMap Method a)           -- Handlers at this node
  , trieStatic    :: !(HashMap Text (RouteTrie a)) -- Static children
  , trieCapture   :: !(Maybe (TypeRep, RouteTrie a)) -- Capture child
  , trieWildcard  :: !(Maybe a)                    -- Wildcard handler
  }

-- | Build a trie from route specs
buildRouteTrie :: [(RouteSpec, a)] -> RouteTrie a
buildRouteTrie = foldr insertRoute emptyTrie

-- | Match a request against the trie
matchTrie :: RouteTrie a -> Method -> [Text] -> Maybe (a, Captures)
matchTrie trie method = go trie []
  where
    go (RouteTrie handlers static capture wildcard) caps = \case
      [] -> (,caps) <$> Map.lookup method handlers
      (seg:rest) ->
        -- Try static match first (more specific)
        (Map.lookup seg static >>= \t -> go t caps rest)
        -- Then try capture
        <|> (capture >>= \(_, t) -> go t ((seg, undefined):caps) rest)
        -- Finally try wildcard
        <|> ((,caps) <$> wildcard)
```

---

## Part 9: Authentication & Authorization

### 9.1 Authentication Directives

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

### 9.2 Authorization

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

## Part 10: Hermes Application Interface (HAI) - Backend Abstraction

WAI (Web Application Interface) has served the Haskell ecosystem well, but it has limitations:
- Headers as `[(CI ByteString, ByteString)]` - inefficient, no type safety
- Tightly coupled to specific representations
- No compile-time header validation
- Limited streaming primitives
- No memory management story for request data lifecycle

Hermes introduces **HAI** (Hermes Application Interface), a next-generation abstraction that:
- Uses Hermes's efficient `HeaderMap` with interned `HeaderFieldName`
- Supports multiple backends (WAI adapter, raw sockets, HTTP/2, QUIC)
- Provides zero-copy operations where possible
- Enables compile-time header direction checking
- **Region-based memory management** - request data lives in arenas, preventing use-after-free
  and enabling bulk deallocation at request end

### 10.1 Core Arena Infrastructure

All request data is allocated in a per-request arena. The region tag `r` ensures
data cannot escape without explicit copying.

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LinearTypes #-}

-- | Request-scoped arena for zero-copy parsing and allocation
data Arena r = Arena
  { arenaPtr      :: {-# UNPACK #-} !(Ptr Word8)
  , arenaCapacity :: {-# UNPACK #-} !Int
  , arenaOffset   :: {-# UNPACK #-} !(IORef Int)
  }

-- | Region-tagged wrapper - data cannot escape its region without copying
newtype Scoped r a = Scoped { unscoped :: a }
  deriving (Functor)

-- | Values that can be copied out of a region (deep copy)
class Copyable a where
  deepCopy :: a -> IO a

instance Copyable ByteString where
  deepCopy = pure . BS.copy

instance Copyable Text where
  deepCopy = pure . T.copy

-- | Escape region by copying - the ONLY way to get data out
escape :: (MonadIO m, Copyable a) => Scoped r a -> m a
escape (Scoped a) = liftIO $ deepCopy a

-- | Run with a request-scoped arena (rank-2 prevents escape)
withRequestArena :: forall a. Int -> (forall r. Arena r -> IO a) -> IO a
withRequestArena size f = bracket allocArena freeArena f
  where
    allocArena = do
      ptr <- mallocBytes size
      ref <- newIORef 0
      pure $ Arena ptr size ref
    freeArena arena = free (arenaPtr arena)

-- | Allocate ByteString in arena (zero-copy into arena memory)
arenaByteString :: Arena r -> ByteString -> IO (Scoped r ByteString)
arenaByteString arena bs = do
  let len = BS.length bs
  offset <- readIORef (arenaOffset arena)
  let newOffset = offset + len
  when (newOffset > arenaCapacity arena) $
    throwIO ArenaOverflow
  writeIORef (arenaOffset arena) newOffset
  let destPtr = arenaPtr arena `plusPtr` offset
  BS.useAsCStringLen bs $ \(srcPtr, srcLen) ->
    copyBytes destPtr (castPtr srcPtr) srcLen
  fp <- newForeignPtr_ destPtr
  pure $ Scoped $ BS.fromForeignPtr fp 0 len
```

### 10.2 Request Type (Region-Scoped)

```haskell
-- | The Hermes Application type - backend agnostic, region-scoped
-- The rank-2 type ensures request data cannot escape
type Application = forall r. Request r -> (Response r -> IO ResponseSent) -> IO ResponseSent

-- | Proof that response was sent (for type safety)
data ResponseSent = ResponseSent

-- | High-performance request representation
-- All ByteString fields are arena-allocated and tagged with region 'r'
data Request r = Request
  { -- Core request line
    requestMethod      :: {-# UNPACK #-} !Method             -- Interned (global, no region)
  , requestPath        :: !(Path r)                          -- Arena-allocated path
  , requestQueryString :: !(QueryString r)                   -- Arena-allocated query
  , requestHttpVersion :: {-# UNPACK #-} !HTTPVersion        -- Packed version (no region)

    -- Headers with Hermes types
  , requestHeaders     :: !(HeaderMap r)                     -- Arena-allocated headers

    -- Body handling
  , requestBody        :: !(RequestBody r)                   -- Arena-scoped streaming
  , requestBodyLength  :: !RequestBodyLength                 -- Known or chunked

    -- Connection info
  , requestRemoteHost  :: !SockAddr                          -- Copied (small, outlives request)
  , requestIsSecure    :: !Bool                              -- TLS?

    -- Arena for additional allocations during handling
  , requestArena       :: !(Arena r)                         -- Handler can allocate here
  }

-- | Efficient path representation using arena memory
data Path r = Path
  { pathSegments   :: {-# UNPACK #-} !(SmallArray (Scoped r Text))  -- Arena-allocated
  , pathRaw        :: !(Scoped r ByteString)                        -- Raw for forwarding
  , pathUnmatched  :: {-# UNPACK #-} !Int                           -- Index of first unmatched
  }

-- | Pre-parsed query string (arena-allocated)
data QueryString r = QueryString
  { queryParams  :: !(HashMap Text (NonEmpty (Scoped r Text)))  -- Keys interned, values in arena
  , queryRaw     :: !(Scoped r ByteString)
  }

-- | Streaming request body with arena integration
data RequestBody r
  = KnownLengthBody {-# UNPACK #-} !Int64 !(IO (Scoped r ByteString))  -- Chunks go to arena
  | ChunkedBody !(IO (Scoped r ByteString))
  | NoBody
```

### 10.3 Response Types (Region-Scoped)

```haskell
-- | High-performance response, region-tagged
-- Response data uses the same arena as the request
data Response r = Response
  { responseStatus  :: {-# UNPACK #-} !StatusCode       -- No region (small)
  , responseHeaders :: !(HeaderMap r)                   -- Arena-allocated headers
  , responseBody    :: !(ResponseBody r)                -- Arena-scoped body
  }

-- | Response body variants - arena-aware
data ResponseBody r
  = BuilderBody !Builder                                -- Builder (will be serialized out)
  | StreamingBody !(StreamingBody r)                    -- Streaming chunks from arena
  | FileBody !FilePath !(Maybe FilePart)                -- Sendfile (no arena, kernel handles)
  | RawBody !(IO ByteString -> (ByteString -> IO ()) -> IO ())  -- Raw takeover

-- | Streaming body type - chunks are arena-scoped
type StreamingBody r = (Builder -> IO ()) -> IO () -> IO ()

-- | Smart constructors with type-safe headers
responseBuilder :: StatusCode -> HeaderMap r -> Builder -> Response r
responseBuilder status hdrs body = Response status hdrs (BuilderBody body)

responseStream :: StatusCode -> HeaderMap r -> StreamingBody r -> Response r
responseStream status hdrs body = Response status hdrs (StreamingBody body)

responseFile :: StatusCode -> HeaderMap r -> FilePath -> Maybe FilePart -> Response r
responseFile status hdrs path part = Response status hdrs (FileBody path part)

-- | Type-safe header setting
setResponseHeader :: forall h r.
  ( KnownHeader h
  , Direction h `AllowedIn` 'ResponseDir
  ) => h -> Response r -> Response r
setResponseHeader h resp = resp { responseHeaders = setHeader h (responseHeaders resp) }

-- | Serialize response for transmission (copies out of arena)
-- This is called at the very end of request handling
serializeResponse :: Response r -> IO RawResponse
serializeResponse resp = do
  -- Headers are small, copy them
  hdrs <- escapeHeaderMap (responseHeaders resp)
  body <- case responseBody resp of
    BuilderBody b -> pure $ RawBuilderBody b
    StreamingBody s -> pure $ RawStreamingBody s
    FileBody path part -> pure $ RawFileBody path part
    RawBody r -> pure $ RawTakeover r
  pure $ RawResponse (responseStatus resp) hdrs body
```

### 10.4 Type-Safe Header Operations

```haskell
-- | Get a request header with compile-time direction check
getRequestHeader :: forall h r.
  ( KnownHeader h
  , Direction h `AllowedIn` 'RequestDir
  ) => Request r -> Either (ParseFailure h) (Maybe h)
getRequestHeader req = lookupHeader @h (requestHeaders req)

-- | Set a response header with compile-time direction check
addResponseHeader :: forall h r.
  ( KnownHeader h
  , Direction h `AllowedIn` 'ResponseDir
  ) => h -> HeaderMap r -> HeaderMap r
addResponseHeader = setHeader

-- | Compile-time error for wrong direction
-- This won't compile:
-- badExample :: Request r -> Maybe SetCookie  -- SetCookie is Response-only!
-- badExample req = getRequestHeader @SetCookie req
-- Error: Header direction mismatch: SetCookie is ResponseDir, not RequestDir

-- | Header presence witness
data HeaderPresent h = HeaderPresent
  { getHeaderValue :: h
  }

-- | Require a header (fails request if missing)
requireHeader :: forall h r.
  ( KnownHeader h
  , Direction h `AllowedIn` 'RequestDir
  ) => Request r -> Either HeaderError (HeaderPresent h)
```

### 10.5 Backend Adapters with Arena Integration

Backends are responsible for:
1. Allocating the per-request arena
2. Parsing raw data into arena-scoped Request
3. Serializing Response out of arena before freeing

```haskell
-- | Type class for backend implementations
class Backend backend where
  type BackendConfig backend :: Type
  type BackendError backend :: Type

  -- | Run an application with this backend and arena pool
  runBackend :: BackendConfig backend
             -> WorkerPool           -- ^ Arena pool for request handling
             -> Application          -- ^ Rank-2 typed application
             -> IO (Either (BackendError backend) ())

  -- | Parse raw request into arena-scoped Request
  parseRequest :: backend -> Arena r -> RawRequest -> IO (Request r)

  -- | Serialize response before arena is freed
  serializeResponse :: backend -> Response r -> IO RawResponse

-- | WAI Backend Adapter
data WAIBackend = WAIBackend

instance Backend WAIBackend where
  type BackendConfig WAIBackend = Warp.Settings
  type BackendError WAIBackend = SomeException

  runBackend settings pool app =
    try $ Warp.runSettings settings (toWaiApp pool app)

  parseRequest _ arena waiReq = do
    -- Parse headers into arena
    headers <- arenaHeaderMap arena (Wai.requestHeaders waiReq)
    -- Parse path into arena
    path <- arenaPath arena (Wai.pathInfo waiReq) (Wai.rawPathInfo waiReq)
    -- Parse query into arena
    query <- arenaQuery arena (Wai.queryString waiReq) (Wai.rawQueryString waiReq)

    pure Request
      { requestMethod = Method $ intern $ Wai.requestMethod waiReq
      , requestPath = path
      , requestQueryString = query
      , requestHeaders = headers
      , requestArena = arena
      , ...
      }

-- | Convert HAI app to WAI app with arena management
toWaiApp :: WorkerPool -> Application -> Wai.Application
toWaiApp pool haiApp waiReq waiRespond = do
  -- Dispatch to pinned worker with arena
  dispatchRequest pool $ \arena -> do
    haiReq <- parseRequest WAIBackend arena waiReq
    haiApp haiReq $ \haiResp -> do
      -- Serialize before arena is freed
      waiResp <- serializeResponse WAIBackend haiResp
      waiRespond waiResp
      pure ResponseSent

-- | Raw Socket Backend with arena (from existing SimpleServer)
data SocketBackend = SocketBackend

instance Backend SocketBackend where
  type BackendConfig SocketBackend = ServerSettings
  type BackendError SocketBackend = IOException

  runBackend settings pool app = runSocketServer settings pool app

  -- Socket backend can do zero-copy parsing directly into arena
  parseRequest _ arena rawBytes = do
    -- flatparse directly into arena memory
    parseHTTPRequestToArena arena rawBytes

-- | HTTP/2 Backend (future)
data HTTP2Backend = HTTP2Backend

instance Backend HTTP2Backend where
  type BackendConfig HTTP2Backend = HTTP2Settings
  type BackendError HTTP2Backend = HTTP2Error
  -- HTTP/2 frames map naturally to arena chunks

-- | QUIC/HTTP3 Backend (future)
data QUICBackend = QUICBackend
```

### 10.6 Performance Optimizations & Allocation Reduction

Hermes prioritizes low-allocation, cache-friendly designs. This section details
strategies for minimizing GC pressure and maximizing throughput.

#### 10.6.1 Off-Heap and Compact Regions

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Compact
import GHC.Prim
import GHC.Types

-- | Long-lived routing tables stored in compact regions
-- Compact regions are not traversed by GC, reducing pause times
data CompactRoutes = CompactRoutes
  { compactTrie    :: !(Compact (RouteTrie Handler))
  , compactStatic  :: !(Compact StaticFileMap)
  }

-- | Build compact routes at startup (one-time cost)
mkCompactRoutes :: RouteTrie Handler -> StaticFileMap -> IO CompactRoutes
mkCompactRoutes trie static = CompactRoutes
  <$> compact trie
  <*> compact static

-- | Access compact data (no copying, pointer into compact region)
lookupRoute :: CompactRoutes -> Method -> Path -> Maybe Handler
lookupRoute cr method path = matchTrie (getCompact $ compactTrie cr) method path

-- | Pinned ByteArrays for headers (avoid copying when sending)
data PinnedByteArray = PinnedByteArray
  { pbaArray  :: !(MutableByteArray# RealWorld)
  , pbaLength :: {-# UNPACK #-} !Int
  }

-- | Allocate pinned memory for response headers
-- Pinned memory can be passed directly to sendmsg() without copying
allocPinnedHeaders :: Int -> IO PinnedByteArray
allocPinnedHeaders size = IO $ \s ->
  case newPinnedByteArray# size s of
    (# s', arr #) -> (# s', PinnedByteArray arr size #)

-- | Write headers directly to pinned buffer
renderHeadersToPinned :: HeaderMap -> PinnedByteArray -> IO Int
renderHeadersToPinned headers pba = do
  -- Write directly to pinned memory, return bytes written
  foldM writeHeader 0 (toHeaderList headers)
  where
    writeHeader offset (name, value) = do
      -- Direct memory writes, no intermediate allocations
      copyToByteArray pba offset (headerNameBytes name)
      ...
```

#### 10.6.2 Buffer Pools and Recycling

```haskell
-- | Pool of reusable buffers to avoid allocation per-request
data BufferPool = BufferPool
  { poolSmall  :: !(Pool SmallBuffer)   -- 4KB buffers for headers
  , poolMedium :: !(Pool MediumBuffer)  -- 64KB buffers for bodies
  , poolLarge  :: !(Pool LargeBuffer)   -- 1MB buffers for uploads
  }

newtype SmallBuffer = SmallBuffer (MutableByteArray RealWorld)
newtype MediumBuffer = MediumBuffer (MutableByteArray RealWorld)
newtype LargeBuffer = LargeBuffer (MutableByteArray RealWorld)

-- | Acquire a buffer, use it, release it back
withSmallBuffer :: BufferPool -> (SmallBuffer -> IO a) -> IO a
withSmallBuffer pool action = bracket
  (acquireBuffer $ poolSmall pool)
  (releaseBuffer $ poolSmall pool)
  action

-- | Request processing with buffer reuse
processRequest :: BufferPool -> Socket -> IO Response
processRequest pool sock = do
  withSmallBuffer pool $ \headerBuf -> do
    -- Read headers into pooled buffer (no allocation)
    bytesRead <- recvBuf sock (bufferPtr headerBuf) 4096
    headers <- parseHeadersFromBuffer headerBuf bytesRead
    ...

-- | Thread-local buffer cache for even lower contention
data ThreadLocalBuffers = ThreadLocalBuffers
  { tlbHeaderBuffer :: {-# UNPACK #-} !(IORef (Maybe SmallBuffer))
  , tlbBodyBuffer   :: {-# UNPACK #-} !(IORef (Maybe MediumBuffer))
  }

-- | Get or allocate thread-local buffer
getThreadLocalHeader :: ThreadLocalBuffers -> IO SmallBuffer
getThreadLocalHeader tlb = do
  mBuf <- readIORef (tlbHeaderBuffer tlb)
  case mBuf of
    Just buf -> pure buf  -- Reuse existing
    Nothing -> do
      buf <- allocSmallBuffer
      writeIORef (tlbHeaderBuffer tlb) (Just buf)
      pure buf
```

#### 10.6.3 Zero-Copy Path and Query Parsing

```haskell
-- | Path that references original request buffer (no copying)
data ZeroCopyPath = ZeroCopyPath
  { zcpSource   :: {-# UNPACK #-} !ByteString  -- Original buffer (kept alive)
  , zcpSegments :: {-# UNPACK #-} !(SmallArray PathSegment)
  }

-- | Path segment as offset+length into source (no allocation per segment)
data PathSegment = PathSegment
  { psOffset :: {-# UNPACK #-} !Int16
  , psLength :: {-# UNPACK #-} !Int16
  }

-- | Parse path without allocating per-segment ByteStrings
parsePathZeroCopy :: ByteString -> ZeroCopyPath
parsePathZeroCopy bs = ZeroCopyPath bs segments
  where
    segments = runST $ do
      arr <- newSmallArray maxSegments undefined
      let go !i !offset
            | offset >= BS.length bs = freezeSmallArray arr 0 i
            | otherwise = do
                let nextSlash = fromMaybe (BS.length bs) $
                      BS.elemIndex '/' (BS.drop offset bs)
                writeSmallArray arr i (PathSegment (fromIntegral offset)
                                                   (fromIntegral $ nextSlash - offset))
                go (i + 1) (nextSlash + 1)
      go 0 1  -- Skip leading /

-- | Get segment text (allocates only when needed, e.g., for capture parsing)
getSegment :: ZeroCopyPath -> Int -> ByteString
getSegment (ZeroCopyPath src segs) i =
  let PathSegment off len = indexSmallArray segs i
  in BS.take (fromIntegral len) $ BS.drop (fromIntegral off) src
{-# INLINE getSegment #-}

-- | Match static segment without allocation (compare in-place)
matchSegment :: ZeroCopyPath -> Int -> ByteString -> Bool
matchSegment zcp i expected =
  let PathSegment off len = indexSmallArray (zcpSegments zcp) i
      actual = BS.take (fromIntegral len) $ BS.drop (fromIntegral off) (zcpSource zcp)
  in actual == expected
{-# INLINE matchSegment #-}
```

#### 10.6.4 Interning and Deduplication

```haskell
-- | Hermes uses symbolize for O(1) header name comparison
-- Header names are interned at startup, pointer equality thereafter

-- | Extended interning for common header values
data InternedValues = InternedValues
  { ivContentTypes :: !(HashMap ByteString Symbol)  -- "application/json" etc.
  , ivMethods      :: !(HashMap ByteString Method)  -- Pre-interned methods
  , ivStatusLines  :: !(SmallArray Builder)         -- Pre-built "HTTP/1.1 200 OK\r\n"
  }

-- | Global interned values (initialized once)
internedValues :: InternedValues
internedValues = unsafePerformIO mkInternedValues
{-# NOINLINE internedValues #-}

mkInternedValues :: IO InternedValues
mkInternedValues = do
  cts <- mapM internContentType commonContentTypes
  InternedValues
    <$> pure (Map.fromList cts)
    <*> pure internedMethods
    <*> mkStatusLines

-- | Pre-built status lines (avoid repeated formatting)
mkStatusLines :: IO (SmallArray Builder)
mkStatusLines = do
  arr <- newSmallArray 600 mempty
  forM_ [100..599] $ \code ->
    writeSmallArray arr code (buildStatusLine code)
  freezeSmallArray arr 0 600

-- | O(1) status line lookup
statusLineBuilder :: StatusCode -> Builder
statusLineBuilder (StatusCode code) =
  indexSmallArray (ivStatusLines internedValues) (fromIntegral code)
{-# INLINE statusLineBuilder #-}
```

#### 10.6.5 Unboxed and Unpacked Data

```haskell
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Route match result without boxing
type RouteMatchResult# = (# (# #) | Handler | Rejection #)
  -- (# (# #) | ... #) is unboxed Maybe - no allocation for Nothing

-- | Unboxed path matching
matchPath# :: RouteTrie -> Path -> Int -> RouteMatchResult#
matchPath# trie path idx
  | idx >= pathLength path = case trieHandler trie of
      Nothing -> (# (# #) | | #)
      Just h  -> (# | h | #)
  | otherwise = ...

-- | Unboxed pair for captures (avoid tuple allocation)
data CaptureResult = CaptureResult
  { crValue  :: {-# UNPACK #-} !Text
  , crNextIdx :: {-# UNPACK #-} !Int
  }

-- | Use unlifted newtypes where possible (GHC 9.2+)
type StatusCode# = Word16#

mkStatus# :: Word16 -> StatusCode#
mkStatus# (W16# w) = w
{-# INLINE mkStatus# #-}
```

#### 10.6.6 Builder Fusion

```haskell
-- | Fused header rendering (single pass, no intermediate structures)
renderHeaders :: HeaderMap -> Builder
renderHeaders = Map.foldMapWithKey renderHeader
  where
    renderHeader name values = foldMap (renderSingleHeader name) values
    {-# INLINE renderHeader #-}

    renderSingleHeader :: HeaderFieldName -> ByteString -> Builder
    renderSingleHeader name value =
      headerNameBuilder name <> colonSpace <> byteString value <> crlf
    {-# INLINE renderSingleHeader #-}

-- | Pre-allocated small builders (avoid thunk allocation)
colonSpace, crlf :: Builder
colonSpace = shortByteString ": "
crlf = shortByteString "\r\n"
{-# NOINLINE colonSpace #-}
{-# NOINLINE crlf #-}

-- | Use shortByteString for small literals (stored inline, no pointer)
statusOK :: Builder
statusOK = shortByteString "HTTP/1.1 200 OK\r\n"
{-# INLINE statusOK #-}
```

#### 10.6.7 Request Recycling

```haskell
-- | Mutable request structure for reuse across keep-alive connections
data MutableRequest s = MutableRequest
  { mrMethod      :: !(STRef s Method)
  , mrPath        :: !(STRef s ZeroCopyPath)
  , mrHeaders     :: !(MutableHeaderMap s)
  , mrBodyBuffer  :: !(MutableByteArray s)
  }

-- | Process multiple requests on same connection with request recycling
handleKeepAlive :: Socket -> MutableRequest RealWorld -> IO ()
handleKeepAlive sock mreq = loop
  where
    loop = do
      -- Parse into mutable request (reuses buffers)
      eof <- parseRequestInto sock mreq
      unless eof $ do
        -- Freeze for handler (cheap, just wraps mutable)
        req <- freezeRequest mreq
        resp <- handleRequest req
        sendResponse sock resp
        -- Reset for next request (no deallocation)
        resetMutableRequest mreq
        loop

-- | Freeze mutable request (O(1), shares underlying memory)
freezeRequest :: MutableRequest RealWorld -> IO Request
freezeRequest MutableRequest{..} = do
  method <- readSTRef mrMethod
  path <- readSTRef mrPath
  headers <- unsafeFreezeHeaderMap mrHeaders
  pure $ Request method path headers ...
```

### 10.5.8 Memory Layout Optimization

```haskell
-- | Cache-line aligned request context (64 bytes on most systems)
data RequestContext = RequestContext
  { rcRequest     :: {-# UNPACK #-} !Request      -- Hot: accessed every request
  , rcPathIndex   :: {-# UNPACK #-} !Int          -- Hot: updated during routing
  , rcMethod      :: {-# UNPACK #-} !Method       -- Hot: checked early
  , rcTracing     :: !(Maybe TracingContext)      -- Cold: only if tracing enabled
  , rcSettings    :: !RouteSettings               -- Cold: rarely accessed
  }

-- | Ensure hot fields are in same cache line
-- Use GHC's inspection-testing to verify layout
{-# ANN type RequestContext (CacheLineAligned 64) #-}

-- | Small, unboxed rejection type (fits in registers)
data Rejection
  = RejPath
  | RejMethod {-# UNPACK #-} !Method
  | RejHeader {-# UNPACK #-} !HeaderFieldName
  | RejAuth
  deriving (Eq)

-- | Use SmallArray for small collections (better cache locality than lists)
type Headers = SmallArray (HeaderFieldName, ByteString)
```

### 10.6 Streaming Primitives

```haskell
-- | Chunked encoding support
chunkedStream :: StreamingBody -> ResponseBody
chunkedStream body = StreamingBody $ \send flush -> do
  body (send . chunkedEncode) flush
  send "0\r\n\r\n"  -- Final chunk

-- | Server-Sent Events helper
sseStream :: (SSEEvent -> IO ()) -> IO () -> StreamingBody
sseStream onEvent cleanup = \send flush -> do
  onEvent $ \event -> do
    send (sseEncode event)
    flush
  cleanup

-- | WebSocket upgrade (raw response takeover)
websocketUpgrade :: (WebSocket -> IO ()) -> Response
websocketUpgrade handler = Response
  { responseStatus = status101
  , responseHeaders = websocketHeaders
  , responseBody = RawBody $ \recv send -> do
      ws <- initWebSocket recv send
      handler ws
  }
```

### 10.7 HAI vs WAI Comparison

| Feature | WAI | HAI (Hermes) |
|---------|-----|--------------|
| Header representation | `[(CI ByteString, ByteString)]` | `HeaderMap` (interned names) |
| Header lookup | O(n) linear scan | O(1) hash lookup |
| Header name comparison | Case-insensitive ByteString | Pointer equality (interned) |
| Type-safe headers | No | Yes (KnownHeader) |
| Direction checking | No | Compile-time |
| Path representation | `[Text]` | `Path` with raw + parsed |
| Query string | Raw `ByteString` | Pre-parsed `HashMap` |
| Method | `ByteString` | Interned `Symbol` |
| Backend support | Warp only (effectively) | Multiple backends |

---

## Part 11: Middleware & Filters

### 11.1 Route Transformers

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

### 11.2 Exception Handling

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

## Part 12: Webmachine-Style HTTP Decision Tree

Inspired by [Webmachine](https://github.com/webmachine/webmachine) (Erlang/OTP), Hermes provides semantic hooks into the HTTP request lifecycle. Instead of ad-hoc middleware, this models HTTP semantics as a decision tree where each decision point corresponds to a specific HTTP semantic.

This approach:
- Makes HTTP semantics explicit and correct by default
- Provides clear extension points for customization
- Ensures proper status codes and headers are returned
- Simplifies reasoning about request handling

### 12.1 The HTTP Decision Tree

```
                                    Request
                                       │
                                       ▼
                              ┌─────────────────┐
                              │ serviceAvailable │───No──▶ 503 Service Unavailable
                              └────────┬────────┘
                                       │Yes
                                       ▼
                              ┌─────────────────┐
                              │  knownMethod    │───No──▶ 501 Not Implemented
                              └────────┬────────┘
                                       │Yes
                                       ▼
                              ┌─────────────────┐
                              │   uriTooLong    │──Yes──▶ 414 URI Too Long
                              └────────┬────────┘
                                       │No
                                       ▼
                              ┌─────────────────┐
                              │  methodAllowed  │───No──▶ 405 Method Not Allowed
                              └────────┬────────┘
                                       │Yes
                                       ▼
                              ┌─────────────────┐
                              │   authorized    │───No──▶ 401 Unauthorized
                              └────────┬────────┘
                                       │Yes
                                       ▼
                              ┌─────────────────┐
                              │   forbidden     │──Yes──▶ 403 Forbidden
                              └────────┬────────┘
                                       │No
                                       ▼
                              ┌─────────────────┐
                              │  contentTypeOk  │───No──▶ 415 Unsupported Media Type
                              └────────┬────────┘
                                       │Yes
                                       ▼
                              ┌─────────────────┐
                              │  acceptExists   │───────▶ Content Negotiation
                              └────────┬────────┘
                                       │
                                       ▼
                              ┌─────────────────┐
                              │ resourceExists  │───No──▶ 404 / POST creates
                              └────────┬────────┘
                                       │Yes
                                       ▼
                              ┌─────────────────┐
                              │  conditionals   │───────▶ 304/412 if applicable
                              └────────┬────────┘
                                       │
                                       ▼
                               Process Request
                                       │
                                       ▼
                                   Response
```

### 12.2 Resource Definition (Region-Scoped)

Resources run in a region-scoped monad `ResourceT r m` which provides access
to arena-allocated request data. The region parameter ensures handlers
cannot hold onto request data past the request lifecycle.

```haskell
-- | Region-scoped resource handler monad
-- Provides access to request data through the arena
newtype ResourceT r m a = ResourceT
  { unResourceT :: ReaderT (ResourceContext r) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Context available during resource handling
data ResourceContext r = ResourceContext
  { resRequest :: !(Request r)     -- ^ Arena-scoped request
  , resArena   :: !(Arena r)       -- ^ Arena for temp allocations
  }

-- | A Resource defines behavior at each HTTP decision point
-- The 'r' parameter tags all request data to the current region
data Resource r m = Resource
  { -- Service availability
    resourceServiceAvailable    :: ResourceT r m Bool

    -- Method handling
  , resourceKnownMethods        :: [Method]  -- Static, no region needed
  , resourceAllowedMethods      :: ResourceT r m [Method]

    -- Authentication & Authorization
  , resourceIsAuthorized        :: ResourceT r m AuthResult
  , resourceForbidden           :: ResourceT r m Bool

    -- Content negotiation
    -- Note: ResponseBody can use arena for building, but is serialized out
  , resourceContentTypesProvided :: ResourceT r m [(MediaType, ResourceT r m (ResponseBody r))]
  , resourceContentTypesAccepted :: ResourceT r m [(MediaType, ResourceT r m ProcessResult)]
  , resourceLanguagesProvided    :: ResourceT r m (Maybe [LanguageTag])
  , resourceCharsetsProvided     :: ResourceT r m (Maybe [Charset])
  , resourceEncodingsProvided    :: ResourceT r m (Maybe [ContentCoding])

    -- Resource existence & lifecycle
  , resourceExists              :: ResourceT r m Bool
  , resourcePreviouslyExisted   :: ResourceT r m Bool
  , resourceMovedPermanently    :: ResourceT r m (Maybe URI)  -- URI is copied
  , resourceMovedTemporarily    :: ResourceT r m (Maybe URI)
  , resourceAllowMissingPost    :: ResourceT r m Bool
  , resourceDeleteResource      :: ResourceT r m Bool
  , resourceDeleteCompleted     :: ResourceT r m Bool
  , resourcePostIsCreate        :: ResourceT r m Bool
  , resourceCreatePath          :: ResourceT r m (Maybe Text)  -- Copied for persistence

    -- Conditional requests (ETags, Last-Modified)
  , resourceGenerateETag        :: ResourceT r m (Maybe ETag)
  , resourceLastModified        :: ResourceT r m (Maybe UTCTime)
  , resourceExpires             :: ResourceT r m (Maybe UTCTime)

    -- Caching
  , resourceOptions             :: ResourceT r m [Header]
  , resourceVariances           :: ResourceT r m [HeaderFieldName]

    -- Multiple representations
  , resourceMultipleChoices     :: ResourceT r m Bool
  }

-- | Access request data within a resource handler
getRequestPath :: ResourceT r m (Scoped r ByteString)
getRequestPath = ResourceT $ asks (pathRaw . requestPath . resRequest)

getRequestBody :: ResourceT r m (RequestBody r)
getRequestBody = ResourceT $ asks (requestBody . resRequest)

-- | Allocate temporary data in the arena
allocInArena :: (Arena r -> IO a) -> ResourceT r m a
allocInArena f = ResourceT $ asks resArena >>= liftIO . f

-- | When data needs to persist (e.g., for database storage), copy it
-- This is the only safe way to escape region-scoped data
persistData :: Copyable a => Scoped r a -> ResourceT r m a
persistData = escape

-- | Default resource with sensible defaults
defaultResource :: Applicative m => Resource r m
defaultResource = Resource
  { resourceServiceAvailable     = pure True
  , resourceKnownMethods         = [mGet, mHead, mPost, mPut, mDelete, mPatch, mOptions]
  , resourceAllowedMethods       = pure [mGet, mHead]
  , resourceIsAuthorized         = pure Authorized
  , resourceForbidden            = pure False
  , resourceContentTypesProvided = pure []
  , resourceContentTypesAccepted = pure []
  , resourceLanguagesProvided    = pure Nothing
  , resourceCharsetsProvided     = pure Nothing
  , resourceEncodingsProvided    = pure Nothing
  , resourceExists               = pure True
  , resourcePreviouslyExisted    = pure False
  , resourceMovedPermanently     = pure Nothing
  , resourceMovedTemporarily     = pure Nothing
  , resourceAllowMissingPost     = pure False
  , resourceDeleteResource       = pure False
  , resourceDeleteCompleted      = pure True
  , resourcePostIsCreate         = pure False
  , resourceCreatePath           = pure Nothing
  , resourceGenerateETag         = pure Nothing
  , resourceLastModified         = pure Nothing
  , resourceExpires              = pure Nothing
  , resourceOptions              = pure []
  , resourceVariances            = pure []
  , resourceMultipleChoices      = pure False
  }
```

### 12.3 Running the Decision Tree

The decision tree runs within the request's region scope, ensuring proper
memory management throughout the HTTP lifecycle.

```haskell
-- | Execute the HTTP decision tree for a resource
-- The rank-2 type ensures the entire decision tree stays within the region
runResource :: forall m a. Monad m
            => (forall r. Resource r m)
            -> (forall r. Request r -> ResourceT r m (Response r))
runResource resource req = runResourceT (ResourceContext req (requestArena req)) $
  runDecisionTree decisions
  where
    decisions = DecisionTree
      { dtServiceAvailable = do
          available <- resourceServiceAvailable resource
          if available then Right <$> continue else pure $ Left status503

      , dtKnownMethod = do
          -- Method is interned (not arena-allocated), safe to access directly
          let method = requestMethod req
          if method `elem` resourceKnownMethods resource
            then Right <$> continue
            else pure $ Left status501

      , dtMethodAllowed = do
          allowed <- resourceAllowedMethods resource
          let method = requestMethod req
          if method `elem` allowed
            then Right <$> continue
            else pure $ Left $ status405WithAllow allowed

      , dtAuthorized = do
          auth <- resourceIsAuthorized resource
          case auth of
            Authorized -> Right <$> continue
            Unauthorized challenge -> pure $ Left $ status401WithChallenge challenge

      , dtForbidden = do
          forbidden <- resourceForbidden resource
          if forbidden
            then pure $ Left status403
            else Right <$> continue

      -- ... remaining decision points
      }

-- | Type-safe decision result
data DecisionResult r
  = Continue                              -- ^ Proceed to next decision
  | Respond !StatusCode ![Header]         -- ^ Short-circuit with response
  | Delegate !(ResourceT r m (Response r)) -- ^ Hand off to resource handler
```

### 12.4 Lifecycle Hooks

```haskell
-- | Hooks that can be attached at any decision point
data LifecycleHooks m = LifecycleHooks
  { -- Pre-decision hooks (can modify request context)
    hookBeforeServiceCheck   :: Request -> m Request
  , hookBeforeMethodCheck    :: Request -> m Request
  , hookBeforeAuth           :: Request -> m Request
  , hookBeforeContentNeg     :: Request -> m Request
  , hookBeforeConditionals   :: Request -> m Request

    -- Post-decision hooks (can observe/log but not modify flow)
  , hookAfterServiceCheck    :: Request -> Bool -> m ()
  , hookAfterMethodCheck     :: Request -> Bool -> m ()
  , hookAfterAuth            :: Request -> AuthResult -> m ()
  , hookAfterContentNeg      :: Request -> Maybe MediaType -> m ()

    -- Response hooks
  , hookBeforeResponse       :: Response -> m Response
  , hookAfterResponse        :: Request -> Response -> m ()

    -- Error hooks
  , hookOnError              :: Request -> SomeException -> m Response
  }

-- | Apply hooks to a resource
withHooks :: Monad m => LifecycleHooks m -> Resource m -> Resource m
withHooks hooks resource = resource
  { resourceServiceAvailable = do
      req <- hookBeforeServiceCheck hooks req
      result <- resourceServiceAvailable resource
      hookAfterServiceCheck hooks req result
      pure result
  -- ... similarly for other decision points
  }

-- | Tracing hook for debugging
tracingHooks :: MonadIO m => LifecycleHooks m
tracingHooks = LifecycleHooks
  { hookAfterServiceCheck = \req result ->
      liftIO $ putStrLn $ "serviceAvailable: " <> show result
  , hookAfterMethodCheck = \req result ->
      liftIO $ putStrLn $ "methodAllowed: " <> show result
  , hookAfterAuth = \req result ->
      liftIO $ putStrLn $ "authorized: " <> show result
  -- ...
  }
```

### 12.5 Conditional Request Handling

```haskell
-- | Full conditional request support (RFC 7232)
data ConditionalResult
  = PreconditionFailed           -- ^ 412: If-Match or If-Unmodified-Since failed
  | NotModified                  -- ^ 304: Resource hasn't changed
  | ProceedWithRequest           -- ^ Continue processing

-- | Check all conditional headers
checkConditionals :: Resource m -> Request -> m ConditionalResult
checkConditionals resource req = do
  etag <- resourceGenerateETag resource
  lastMod <- resourceLastModified resource

  -- Check If-Match (for PUT/PATCH/DELETE)
  case getRequestHeader @IfMatch req of
    Just (IfMatch tags) ->
      unless (matchesETag etag tags) $
        return PreconditionFailed

  -- Check If-None-Match (for GET/HEAD - caching)
  case getRequestHeader @IfNoneMatch req of
    Just (IfNoneMatch tags) ->
      when (matchesETag etag tags) $
        return NotModified

  -- Check If-Modified-Since (for GET/HEAD)
  case getRequestHeader @IfModifiedSince req of
    Just (IfModifiedSince since) ->
      when (maybe False (<= since) lastMod) $
        return NotModified

  -- Check If-Unmodified-Since (for PUT/PATCH/DELETE)
  case getRequestHeader @IfUnmodifiedSince req of
    Just (IfUnmodifiedSince since) ->
      when (maybe False (> since) lastMod) $
        return PreconditionFailed

  return ProceedWithRequest

-- | Automatically add ETag and Last-Modified to responses
addConditionalHeaders :: Resource m -> Response -> m Response
addConditionalHeaders resource resp = do
  etag <- resourceGenerateETag resource
  lastMod <- resourceLastModified resource
  return $ resp
    & maybe id (setResponseHeader . ETag) etag
    & maybe id (setResponseHeader . LastModified) lastMod
```

### 12.6 Content Negotiation Engine

```haskell
-- | Full conneg implementation (RFC 7231)
data NegotiationResult = NegotiationResult
  { negotiatedMediaType  :: !MediaType
  , negotiatedLanguage   :: !(Maybe LanguageTag)
  , negotiatedCharset    :: !(Maybe Charset)
  , negotiatedEncoding   :: !(Maybe ContentCoding)
  }

-- | Negotiate content type, language, charset, and encoding
negotiate :: Resource m -> Request -> m (Either StatusCode NegotiationResult)
negotiate resource req = do
  -- Get what the resource can provide
  mediaTypes <- resourceContentTypesProvided resource
  languages  <- resourceLanguagesProvided resource
  charsets   <- resourceCharsetsProvided resource
  encodings  <- resourceEncodingsProvided resource

  -- Get what the client accepts
  let accept     = getRequestHeader @Accept req
      acceptLang = getRequestHeader @AcceptLanguage req
      acceptChar = getRequestHeader @AcceptCharset req
      acceptEnc  = getRequestHeader @AcceptEncoding req

  -- Perform negotiation
  case selectMediaType accept (map fst mediaTypes) of
    Nothing -> return $ Left status406  -- Not Acceptable
    Just mt -> do
      let lang = selectLanguage acceptLang =<< languages
          char = selectCharset acceptChar =<< charsets
          enc  = selectEncoding acceptEnc =<< encodings
      return $ Right NegotiationResult
        { negotiatedMediaType = mt
        , negotiatedLanguage  = lang
        , negotiatedCharset   = char
        , negotiatedEncoding  = enc
        }

-- | Add Vary header based on what was negotiated
addVaryHeader :: NegotiationResult -> Response -> Response
addVaryHeader neg = setResponseHeader $ Vary $ catMaybes
  [ Just hAccept
  , hAcceptLanguage <$ negotiatedLanguage neg
  , hAcceptCharset <$ negotiatedCharset neg
  , hAcceptEncoding <$ negotiatedEncoding neg
  ]
```

### 12.7 Integration with Route DSL

```haskell
-- | Embed a Resource in a route
resource :: Monad m => Resource m -> RouteT e m Response
resource res = RouteT $ \ctx -> do
  response <- runResource res (rcRequest ctx)
  pure $ Matched response

-- | Define a resource inline with the route DSL
userResource :: Int -> Resource Handler
userResource userId = defaultResource
  { resourceAllowedMethods = pure [mGet, mPut, mDelete]

  , resourceExists = do
      mUser <- Database.lookup userId
      pure $ isJust mUser

  , resourceContentTypesProvided = pure
      [ (mediaTypeJson, Json <$> Database.lookup userId)
      , (mediaTypeXml,  Xml  <$> Database.lookup userId)
      ]

  , resourceContentTypesAccepted = pure
      [ (mediaTypeJson, do
          body <- getRequestBody
          case eitherDecodeStrict body of
            Left err -> pure $ ProcessError err
            Right user -> do
              Database.update userId user
              pure ProcessSucceeded)
      ]

  , resourceDeleteResource = do
      Database.delete userId
      pure True

  , resourceGenerateETag = do
      mUser <- Database.lookup userId
      pure $ fmap (etagFromHash . hash) mUser

  , resourceLastModified = do
      mUser <- Database.lookup userId
      pure $ fmap userModifiedAt mUser
  }

-- | Use in routes
routes :: RouteT ServerError Handler Response
routes =
  path "users" $
    (get >> pathEnd >> resource usersListResource)
    <|> (capture >>= \uid -> resource (userResource uid))
```

### 12.8 Decision Tree Visualization (Debug Mode)

```haskell
-- | Generate a visualization of decisions made for a request
data DecisionTrace = DecisionTrace
  { traceDecisions :: [(Text, Bool, Maybe StatusCode)]
  , traceNegotiation :: Maybe NegotiationResult
  , traceFinalStatus :: StatusCode
  , traceElapsedTime :: NominalDiffTime
  }

-- | Enable tracing for debugging
withTracing :: MonadIO m => Resource m -> Resource (TracingT m)
withTracing resource = ...

-- | Pretty-print a decision trace
renderTrace :: DecisionTrace -> Text
renderTrace trace = T.unlines $
  [ "HTTP Decision Trace"
  , "==================="
  ] ++
  [ (if passed then "✓" else "✗") <> " " <> name <>
    maybe "" ((" → " <>) . T.pack . show) status
  | (name, passed, status) <- traceDecisions trace
  ] ++
  [ ""
  , "Final: " <> T.pack (show $ traceFinalStatus trace)
  , "Time: " <> T.pack (show $ traceElapsedTime trace)
  ]
```

### 12.9 Rewrite Rules and Fusion for Resources

GHC rewrite rules enable zero-cost abstractions by eliminating intermediate
structures at compile time. This is especially valuable for the decision tree
where naive implementations would allocate per-decision.

#### 12.9.1 Decision Fusion

```haskell
{-# LANGUAGE RankNTypes #-}

-- | Representation for fusion: CPS-style decisions
newtype DecisionM m a = DecisionM
  { runDecisionM :: forall r.
      (a -> m r)           -- Continue
      -> (Response -> m r) -- Short-circuit
      -> m r
  }

instance Functor (DecisionM m) where
  fmap f (DecisionM g) = DecisionM $ \cont short ->
    g (cont . f) short
  {-# INLINE fmap #-}

instance Applicative (DecisionM m) where
  pure a = DecisionM $ \cont _ -> cont a
  {-# INLINE pure #-}
  DecisionM f <*> DecisionM a = DecisionM $ \cont short ->
    f (\fab -> a (\x -> cont (fab x)) short) short
  {-# INLINE (<*>) #-}

instance Monad (DecisionM m) where
  DecisionM m >>= f = DecisionM $ \cont short ->
    m (\a -> runDecisionM (f a) cont short) short
  {-# INLINE (>>=) #-}

-- | Short-circuit with response
shortCircuit :: Response -> DecisionM m a
shortCircuit resp = DecisionM $ \_ short -> short resp
{-# INLINE shortCircuit #-}

-- | Lift an action
liftDecision :: Monad m => m a -> DecisionM m a
liftDecision ma = DecisionM $ \cont _ -> ma >>= cont
{-# INLINE liftDecision #-}

-- | RULE: Fuse consecutive decisions
{-# RULES
"decision/bind-assoc" forall m f g.
  (m >>= f) >>= g = m >>= (\x -> f x >>= g)

"decision/pure-bind" forall a f.
  pure a >>= f = f a

"decision/bind-pure" forall m.
  m >>= pure = m

"decision/fmap-pure" forall f a.
  fmap f (pure a) = pure (f a)
#-}
```

#### 12.9.2 Resource Field Fusion

```haskell
-- | Build resource with static analysis opportunities
data ResourceBuilder m = ResourceBuilder
  { rbServiceAvailable :: First (m Bool)
  , rbAllowedMethods   :: First (m [Method])
  , rbExists           :: First (m Bool)
  , rbContentTypes     :: First (m [(MediaType, m ResponseBody)])
  -- ... other fields as First to enable Monoid-based merging
  }

instance Semigroup (ResourceBuilder m) where
  a <> b = ResourceBuilder
    { rbServiceAvailable = rbServiceAvailable a <> rbServiceAvailable b
    , rbAllowedMethods = rbAllowedMethods a <> rbAllowedMethods b
    -- ...
    }

-- | RULE: Fuse resource building
{-# RULES
"resource/mempty-left" forall r.
  mempty <> r = r

"resource/mempty-right" forall r.
  r <> mempty = r

"resource/assoc" forall a b c.
  (a <> b) <> c = a <> (b <> c)
#-}

-- | Finalize resource (fuses all overrides)
finalizeResource :: Applicative m => ResourceBuilder m -> Resource m
finalizeResource rb = defaultResource
  { resourceServiceAvailable = fromMaybe (pure True) $ getFirst $ rbServiceAvailable rb
  , resourceAllowedMethods = fromMaybe (pure [mGet, mHead]) $ getFirst $ rbAllowedMethods rb
  -- ...
  }
{-# INLINE finalizeResource #-}
```

#### 12.9.3 Decision Tree Specialization

```haskell
-- | Specialize decision tree based on resource configuration
-- If resource always returns True for serviceAvailable, skip that check

class KnownDecision (d :: Bool) where
  skipDecision :: proxy d -> Bool

instance KnownDecision 'True where
  skipDecision _ = True
  {-# INLINE skipDecision #-}

instance KnownDecision 'False where
  skipDecision _ = False
  {-# INLINE skipDecision #-}

-- | Type-level resource with known static decisions
data StaticResource (serviceAvailable :: Bool)
                    (authRequired :: Bool)
                    m = StaticResource (Resource m)

-- | RULE: Eliminate static True checks
{-# RULES
"decision/service-true" forall r.
  checkServiceAvailable (StaticResource @'True @auth r) = pure ()

"decision/auth-false" forall r.
  checkAuthorization (StaticResource @sa @'False r) = pure Authorized
#-}

-- | Specialize at compile time
runStaticResource :: forall sa auth m.
  (KnownDecision sa, KnownDecision auth, Monad m)
  => StaticResource sa auth m -> Request -> m Response
runStaticResource (StaticResource res) req = runDecisionM decisions pure id
  where
    decisions = do
      -- These checks are eliminated by RULES when statically known
      unless (skipDecision (Proxy @sa)) $
        unlessM (liftDecision $ resourceServiceAvailable res) $
          shortCircuit response503

      unless (skipDecision (Proxy @auth)) $ do
        auth <- liftDecision $ resourceIsAuthorized res
        case auth of
          Authorized -> pure ()
          Unauthorized c -> shortCircuit $ response401 c

      -- Continue with remaining decisions
      ...
{-# INLINE runStaticResource #-}
```

#### 12.9.4 Stream Fusion for Response Bodies

```haskell
-- | Fused stream type (like vector's Bundle)
data Stream m a = forall s. Stream
  (s -> m (Step s a))  -- Stepper
  s                     -- Initial state
  Size                  -- Size hint

data Step s a
  = Yield !a !s
  | Skip !s
  | Done

-- | RULE: Fuse map/filter chains
{-# RULES
"stream/map-map" forall f g s.
  mapS f (mapS g s) = mapS (f . g) s

"stream/filter-filter" forall p q s.
  filterS p (filterS q s) = filterS (\x -> p x && q x) s

"stream/map-filter" forall f p s.
  filterS p (mapS f s) = mapFilterS f p s
#-}

-- | Fused response body rendering
renderBodyFused :: ResponseBody -> Stream IO Builder
renderBodyFused (BuilderBody b) = singleton b
renderBodyFused (StreamingBody sb) = streamToFused sb

-- | Convert to final ByteString with fusion
toByteString :: Stream IO Builder -> IO ByteString
toByteString = foldStream (<>) mempty >=> pure . toLazyByteString
{-# INLINE toByteString #-}
```

#### 12.9.5 Inlining Control

```haskell
-- | Decision functions with carefully controlled inlining
-- INLINE: Small, hot functions that benefit from specialization
-- INLINABLE: Functions that should be specializable but not always inlined
-- NOINLINE: Functions that shouldn't be duplicated (large or cold)

checkServiceAvailable :: Monad m => Resource m -> DecisionM m ()
checkServiceAvailable res = do
  available <- liftDecision $ resourceServiceAvailable res
  unless available $ shortCircuit response503
{-# INLINE checkServiceAvailable #-}  -- Small, always inline

runContentNegotiation :: Monad m => Resource m -> Request -> DecisionM m NegotiationResult
runContentNegotiation res req = ...
{-# INLINABLE runContentNegotiation #-}  -- Specialize per resource, but not tiny

generateErrorResponse :: StatusCode -> Text -> Response
generateErrorResponse = ...
{-# NOINLINE generateErrorResponse #-}  -- Cold path, don't duplicate

-- | Phase control for rules
{-# INLINE [1] mapResource #-}
{-# INLINE [1] bindResource #-}
{-# RULES
"resource/map-bind" [2] forall f g m.
  mapResource f (bindResource m g) = bindResource m (mapResource f . g)
#-}
```

#### 12.9.6 Inspection Testing

```haskell
-- | Verify fusion happens using inspection-testing
{-# LANGUAGE TemplateHaskell #-}

import Test.Inspection

-- | This should compile to a single loop with no intermediate allocations
processResourceFused :: Resource IO -> Request -> IO Response
processResourceFused res req = runDecisionM (allDecisions res req) pure id

-- | Verify no dictionaries remain (full specialization)
inspect $ hasNoTypeClasses 'processResourceFused

-- | Verify no allocations in hot path
inspect $ 'processResourceFused `doesNotUse` 'GHC.Base.build
inspect $ 'processResourceFused `doesNotUse` 'GHC.Base.foldr

-- | Verify specific rules fired
inspect $ 'processResourceFused `hasRule` "decision/bind-assoc"
```

#### 12.9.7 Template Haskell for Static Resources

```haskell
-- | Generate fully specialized resource handlers at compile time
mkResource :: Name -> Q [Dec]
mkResource name = do
  -- Analyze resource definition
  info <- reify name
  let decisions = analyzeResourceDecisions info

  -- Generate specialized code path
  [d|
    $(varP $ mkName $ "run_" ++ nameBase name) :: Request -> IO Response
    $(varP $ mkName $ "run_" ++ nameBase name) = \req -> do
      $(generateSpecializedDecisions decisions)
  |]

-- | Generate decision code, eliminating statically-known checks
generateSpecializedDecisions :: [Decision] -> Q Exp
generateSpecializedDecisions decisions = do
  -- Skip checks that are statically True
  let activeDecisions = filter (not . isStaticallyTrue) decisions

  -- Generate code for remaining decisions
  foldr chainDecision [| pure |] activeDecisions
  where
    chainDecision d rest = [|
      do result <- $(decisionCode d)
         case result of
           Continue -> $rest
           Respond resp -> pure resp
      |]

-- | Usage:
$(mkResource 'userResource)
-- Generates: run_userResource :: Request -> IO Response
-- With all static checks eliminated
```

---

## Part 13: OpenTelemetry Tracing

Hermes provides first-class [OpenTelemetry](https://opentelemetry.io/) integration following
the [HTTP semantic conventions](https://opentelemetry.io/docs/specs/semconv/http/http-spans/).
Tracing is built into the framework rather than bolted on as middleware, enabling:
- Accurate `http.route` attributes (from the routing layer)
- Proper span parenting through async operations
- Automatic context propagation
- Integration with the Webmachine decision tree

### 13.1 HTTP Server Spans

Following the [OTel HTTP server span conventions](https://opentelemetry.io/docs/specs/semconv/http/http-spans/):

```haskell
-- | OpenTelemetry span attributes for HTTP servers
data HTTPServerSpanAttributes = HTTPServerSpanAttributes
  { -- Required attributes
    httpRequestMethod     :: !Method              -- http.request.method
  , urlScheme             :: !Text                -- url.scheme (http/https)
  , urlPath               :: !Text                -- url.path

    -- Conditionally required
  , httpRoute             :: !(Maybe Text)        -- http.route (MUST be low cardinality!)
  , httpResponseStatusCode :: !(Maybe StatusCode) -- http.response.status_code
  , errorType             :: !(Maybe Text)        -- error.type (on errors)

    -- Recommended
  , serverAddress         :: !(Maybe Text)        -- server.address
  , serverPort            :: !(Maybe Int)         -- server.port
  , urlQuery              :: !(Maybe Text)        -- url.query (sanitized!)
  , userAgentOriginal     :: !(Maybe Text)        -- user_agent.original
  , clientAddress         :: !(Maybe Text)        -- client.address
  , clientPort            :: !(Maybe Int)         -- client.port

    -- Network attributes
  , networkProtocolName   :: !(Maybe Text)        -- network.protocol.name
  , networkProtocolVersion :: !(Maybe Text)       -- network.protocol.version
  }

-- | Span naming follows OTel conventions: "{method} {route}" or just "{method}"
spanName :: HTTPServerSpanAttributes -> Text
spanName attrs = case httpRoute attrs of
  Just route -> T.unwords [renderMethod (httpRequestMethod attrs), route]
  Nothing    -> renderMethod (httpRequestMethod attrs)

-- | Extract span attributes from HAI Request
extractServerSpanAttributes :: Request -> HTTPServerSpanAttributes
extractServerSpanAttributes req = HTTPServerSpanAttributes
  { httpRequestMethod = requestMethod req
  , urlScheme = if requestIsSecure req then "https" else "http"
  , urlPath = pathRaw (requestPath req)
  , httpRoute = Nothing  -- Set by routing layer!
  , httpResponseStatusCode = Nothing  -- Set after response
  , errorType = Nothing
  , serverAddress = Nothing  -- From Host header
  , serverPort = Nothing
  , urlQuery = Just $ queryRaw (requestQueryString req)
  , userAgentOriginal = getHeaderText @UserAgent req
  , clientAddress = Just $ renderSockAddr (requestRemoteHost req)
  , clientPort = sockAddrPort (requestRemoteHost req)
  , networkProtocolName = Just "http"
  , networkProtocolVersion = Just $ renderHTTPVersion (requestHttpVersion req)
  }
```

### 13.2 Route-Aware Tracing

The key advantage of framework-integrated tracing is access to `http.route`:

```haskell
-- | Tracing context carried through routing
data TracingContext = TracingContext
  { tcSpan        :: !Span                    -- Current span
  , tcRoute       :: !(IORef (Maybe Text))    -- Route pattern (set by routing)
  , tcAttributes  :: !(IORef [(Text, AttributeValue)]) -- Additional attributes
  }

-- | Middleware that creates the server span
withServerSpan :: Tracer -> Middleware
withServerSpan tracer app req respond = do
  let attrs = extractServerSpanAttributes req
      initialName = spanName attrs  -- Just method initially

  inSpan tracer initialName (spanOpts attrs) $ \span -> do
    -- Create mutable route ref - routing layer will fill this in
    routeRef <- newIORef Nothing
    attrsRef <- newIORef []

    let ctx = TracingContext span routeRef attrsRef
        req' = req { requestTracingContext = Just ctx }

    app req' $ \resp -> do
      -- Update span with final attributes
      mRoute <- readIORef routeRef
      extraAttrs <- readIORef attrsRef

      -- Update span name if we have a route
      forM_ mRoute $ \route -> do
        updateSpanName span (renderMethod (httpRequestMethod attrs) <> " " <> route)
        setAttributes span [("http.route", toAttribute route)]

      -- Set response attributes
      setAttributes span $
        [ ("http.response.status_code", toAttribute $ statusCodeInt $ responseStatus resp)
        ] <> extraAttrs

      -- Set error status if applicable
      when (isErrorStatus $ responseStatus resp) $
        setStatus span (Error $ statusMessage $ responseStatus resp)

      respond resp

  where
    spanOpts attrs = defaultSpanArguments
      { kind = Server
      , attributes = toOTelAttributes attrs
      }

-- | Route directive that records the matched route pattern
recordRoute :: Text -> RouteT e m ()
recordRoute pattern = RouteT $ \ctx -> do
  -- Record the route pattern for tracing
  forM_ (requestTracingContext $ rcRequest ctx) $ \tc ->
    writeIORef (tcRoute tc) (Just pattern)
  pure $ Matched ()

-- | Path matching that automatically records the route
pathWithTrace :: Text -> RouteT e m a -> RouteT e m a
pathWithTrace segment inner = do
  path segment
  recordRouteSegment segment
  inner
```

### 13.3 Client Span Support

```haskell
-- | HTTP client span attributes
data HTTPClientSpanAttributes = HTTPClientSpanAttributes
  { clientHttpRequestMethod :: !Method        -- http.request.method
  , clientUrlFull           :: !Text          -- url.full
  , clientServerAddress     :: !Text          -- server.address
  , clientServerPort        :: !Int           -- server.port
  , clientHttpResponseStatus :: !(Maybe StatusCode)
  , clientErrorType         :: !(Maybe Text)
  }

-- | Client middleware for outgoing requests
withClientSpan :: Tracer -> ClientMiddleware
withClientSpan tracer makeRequest req = do
  let attrs = extractClientAttrs req
      name = renderMethod (clientHttpRequestMethod attrs)

  inSpan tracer name (clientSpanOpts attrs) $ \span -> do
    -- Inject trace context into request headers
    ctx <- getContext
    req' <- injectContext ctx req

    -- Make the request
    resp <- makeRequest req'

    -- Record response
    setAttributes span
      [ ("http.response.status_code", toAttribute $ statusCodeInt $ responseStatus resp)
      ]

    when (isErrorStatus $ responseStatus resp) $
      setStatus span (Error "HTTP error")

    pure resp

-- | Inject W3C Trace Context headers
injectContext :: Context -> Request -> IO Request
injectContext ctx req = do
  let headers = requestHeaders req
      traceparent = renderTraceparent (spanContext $ getSpan ctx)
      tracestate = renderTracestate ctx
  pure $ req
    { requestHeaders = setHeader (TraceparentHeader traceparent) $
                       setHeader (TracestateHeader tracestate) headers
    }
```

### 13.4 Integration with Webmachine Decision Tree

The decision tree provides natural tracing points:

```haskell
-- | Traced resource with automatic span events
data TracedResource m = TracedResource
  { tracedResource :: Resource m
  , resourceTracer :: Tracer
  }

-- | Run resource with decision tracing
runTracedResource :: MonadIO m => TracedResource m -> Request -> m Response
runTracedResource TracedResource{..} req = do
  let span = getSpanFromRequest req

  -- Add span events for each decision point
  addEvent span "http.decision.service_available"
  available <- resourceServiceAvailable tracedResource
  setAttributes span [("hermes.decision.service_available", toAttribute available)]

  unless available $ do
    addEvent span "http.decision.rejected" [("reason", "service_unavailable")]
    pure $ responseServiceUnavailable

  addEvent span "http.decision.method_check"
  -- ... continue through decision tree

  -- Final response
  addEvent span "http.decision.complete"
  resp <- generateResponse
  pure resp

-- | Lifecycle hooks that emit trace events
tracingHooks :: Tracer -> LifecycleHooks m
tracingHooks tracer = LifecycleHooks
  { hookAfterServiceCheck = \req result -> do
      let span = getSpanFromRequest req
      addEvent span "hermes.service_check" [("result", toAttribute result)]

  , hookAfterMethodCheck = \req result -> do
      let span = getSpanFromRequest req
      addEvent span "hermes.method_check"
        [ ("result", toAttribute result)
        , ("method", toAttribute $ requestMethod req)
        ]

  , hookAfterAuth = \req result -> do
      let span = getSpanFromRequest req
      addEvent span "hermes.auth_check"
        [("result", toAttribute $ show result)]
      -- Don't log sensitive auth details!

  , hookAfterContentNeg = \req mMediaType -> do
      let span = getSpanFromRequest req
      forM_ mMediaType $ \mt ->
        setAttributes span [("http.response.content_type", toAttribute $ renderMediaType mt)]

  , hookOnError = \req ex -> do
      let span = getSpanFromRequest req
      recordException span ex
      setStatus span (Error $ T.pack $ show ex)
      pure $ responseInternalError
  }
```

### 13.5 Context Propagation

```haskell
-- | W3C Trace Context headers (RFC trace-context)
newtype TraceparentHeader = TraceparentHeader Text
  deriving (Eq, Show)

instance KnownHeader TraceparentHeader where
  type ParseFailure TraceparentHeader = Text
  type Cardinality TraceparentHeader = 'ZeroOrOne
  type Direction TraceparentHeader = 'Request
  headerName _ = "traceparent"
  parseFromHeaders _ (bs :| _) = parseTraceparent (decodeUtf8 bs)
  renderToHeaders _ (TraceparentHeader t) = encodeUtf8 t

newtype TracestateHeader = TracestateHeader Text
  deriving (Eq, Show)

instance KnownHeader TracestateHeader where
  type ParseFailure TracestateHeader = Text
  type Cardinality TracestateHeader = 'ZeroOrOne
  type Direction TracestateHeader = 'Request
  headerName _ = "tracestate"
  parseFromHeaders _ (bs :| _) = Right $ TracestateHeader $ decodeUtf8 bs
  renderToHeaders _ (TracestateHeader t) = encodeUtf8 t

-- | Extract parent context from incoming request
extractParentContext :: Request -> IO (Maybe SpanContext)
extractParentContext req = do
  case getRequestHeader @TraceparentHeader req of
    Right (Just (TraceparentHeader tp)) -> parseTraceparent tp
    _ -> pure Nothing

-- | Automatic context propagation middleware
contextPropagationMiddleware :: Middleware
contextPropagationMiddleware app req respond = do
  mParentCtx <- extractParentContext req
  case mParentCtx of
    Nothing -> app req respond  -- No parent, create root span
    Just parentCtx -> do
      -- Set parent context for child span creation
      withParentContext parentCtx $
        app req respond
```

### 13.6 Metrics Integration

Following [OTel HTTP metrics conventions](https://opentelemetry.io/docs/specs/semconv/http/http-metrics/):

```haskell
-- | HTTP server metrics
data HTTPServerMetrics = HTTPServerMetrics
  { requestDuration :: !Histogram   -- http.server.request.duration
  , activeRequests  :: !UpDownCounter -- http.server.active_requests
  , requestSize     :: !Histogram   -- http.server.request.body.size
  , responseSize    :: !Histogram   -- http.server.response.body.size
  }

-- | Create metrics with standard names
mkHTTPServerMetrics :: Meter -> IO HTTPServerMetrics
mkHTTPServerMetrics meter = HTTPServerMetrics
  <$> createHistogram meter "http.server.request.duration"
        (histogramOpts { unit = Just "s", description = Just "Duration of HTTP server requests" })
  <*> createUpDownCounter meter "http.server.active_requests"
        (counterOpts { description = Just "Number of active HTTP server requests" })
  <*> createHistogram meter "http.server.request.body.size"
        (histogramOpts { unit = Just "By" })
  <*> createHistogram meter "http.server.response.body.size"
        (histogramOpts { unit = Just "By" })

-- | Metrics middleware
metricsMiddleware :: HTTPServerMetrics -> Middleware
metricsMiddleware metrics app req respond = do
  -- Track active requests
  add (activeRequests metrics) 1 (requestAttrs req)

  start <- getCurrentTime
  app req $ \resp -> do
    end <- getCurrentTime
    let duration = realToFrac $ diffUTCTime end start

    -- Record metrics
    record (requestDuration metrics) duration (responseAttrs req resp)
    add (activeRequests metrics) (-1) (requestAttrs req)

    forM_ (requestBodyLength req) $ \len ->
      record (requestSize metrics) (fromIntegral len) (requestAttrs req)

    forM_ (getResponseBodyLength resp) $ \len ->
      record (responseSize metrics) (fromIntegral len) (responseAttrs req resp)

    respond resp

  where
    requestAttrs req =
      [ ("http.request.method", toAttribute $ requestMethod req)
      , ("url.scheme", toAttribute $ if requestIsSecure req then "https" else "http")
      ]
    responseAttrs req resp = requestAttrs req <>
      [ ("http.response.status_code", toAttribute $ statusCodeInt $ responseStatus resp)
      , ("http.route", maybe "" id $ getMatchedRoute req)
      ]
```

### 13.7 Configuration

```haskell
-- | OTel configuration for Hermes
data OTelConfig = OTelConfig
  { otelServiceName    :: !Text           -- service.name
  , otelServiceVersion :: !(Maybe Text)   -- service.version
  , otelEnvironment    :: !(Maybe Text)   -- deployment.environment
  , otelTracerProvider :: !TracerProvider
  , otelMeterProvider  :: !MeterProvider
  , otelPropagators    :: ![Propagator]   -- Context propagators
  , otelSampler        :: !Sampler        -- Sampling strategy
  }

-- | Default OTel config with sensible defaults
defaultOTelConfig :: IO OTelConfig
defaultOTelConfig = do
  -- Respect OTEL_* environment variables
  serviceName <- fromMaybe "hermes" <$> lookupEnv "OTEL_SERVICE_NAME"
  tracerProvider <- initTracerProvider
  meterProvider <- initMeterProvider
  pure OTelConfig
    { otelServiceName = T.pack serviceName
    , otelServiceVersion = Nothing
    , otelEnvironment = Nothing
    , otelTracerProvider = tracerProvider
    , otelMeterProvider = meterProvider
    , otelPropagators = [w3cTraceContext, w3cBaggage]
    , otelSampler = parentBasedSampler alwaysOn
    }

-- | Run server with full OTel instrumentation
runServerWithOTel :: Backend b
                  => OTelConfig
                  -> BackendConfig b
                  -> RouteT ServerError IO Response
                  -> IO ()
runServerWithOTel config backendConfig routes = do
  let tracer = makeTracer (otelTracerProvider config) "hermes"
      meter = makeMeter (otelMeterProvider config) "hermes"

  metrics <- mkHTTPServerMetrics meter

  let app = toHAIApplication routes
      instrumented = withServerSpan tracer
                   . metricsMiddleware metrics
                   . contextPropagationMiddleware
                   $ app

  runBackend backendConfig instrumented
```

---

## Part 14: Running Routes

### 14.1 HAI Integration

```haskell
-- | Convert a route to a HAI Application
toHAIApplication :: RouteT ServerError IO Response -> HAI.Application
toHAIApplication route request respond = do
  let ctx = RequestContext
        { rcRequest = request
        , rcUnmatchedPath = pathSegments (requestPath request)
        , rcHeaders = requestHeaders request  -- Already a HeaderMap!
        , rcSettings = defaultRouteSettings
        }
  result <- runRouteT route ctx
  respond $ case result of
    Matched resp -> resp
    Rejected rej -> rejectionResponse rej
    Failed err   -> errorResponse err

-- | Run with any backend
runServer :: Backend b => BackendConfig b -> RouteT ServerError IO Response -> IO ()
runServer config route = runBackend config (toHAIApplication route)

-- | Convenience for WAI/Warp (common case)
runWarp :: Warp.Settings -> RouteT ServerError IO Response -> IO ()
runWarp = runServer @WAIBackend

-- | Convenience for raw sockets (development/testing)
runSocket :: ServerSettings -> RouteT ServerError IO Response -> IO ()
runSocket = runServer @SocketBackend
```

### 13.2 Error Responses

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

## Part 15: Testing Support

### 15.1 Route Testing DSL

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

### 15.2 Property-Based Testing

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

## Part 16: Implementation Phases

### Phase 1: HAI Core & Backend Abstraction
- [ ] Define HAI `Request` type with `HeaderMap`, interned `Method`, efficient `Path`
- [ ] Define HAI `Response` type with type-safe headers
- [ ] Zero-copy path parsing (`ZeroCopyPath`, `PathSegment`)
- [ ] Buffer pools for request/response handling
- [ ] Pinned memory for sendmsg() optimization
- [ ] Implement `Backend` type class
- [ ] Create WAI adapter (`toWaiApp`, `fromWaiApp`)
- [ ] Port `SimpleServer` to be a HAI backend
- [ ] Benchmark HAI vs WAI overhead

### Phase 2: Core Route Monad
- [ ] Implement `RouteT` monad with HAI types
- [ ] Basic path matching (`path`, `pathEnd`, `capture`)
- [ ] Method matching using Hermes `Method` type
- [ ] Route composition (`<|>`, `</>`)
- [ ] Integration with HAI `Application` type
- [ ] Runtime route overlap detection (`RouteSpec`, `RouteAnalysis`)
- [ ] Route trie construction for efficient matching
- [ ] Startup validation with `runServerWithValidation`

### Phase 3: Header & Body Integration
- [ ] Header directives with `KnownHeader` and direction checking
- [ ] Compile-time header direction validation
- [ ] Content negotiation using existing Hermes primitives
- [ ] Body parsing/rendering type classes
- [ ] JSON, form, and multipart support

### Phase 4: Template Haskell Support
- [ ] Route quasi-quoter (`[routes|...|]`)
- [ ] Compile-time route validation (conflicts, unreachable routes)
- [ ] Compile-time route optimization (trie generation)
- [ ] Reverse routing / URL generation
- [ ] Handler type signature derivation
- [ ] Improved compile-time error messages

### Phase 5: Query Parameters & Auth
- [ ] Query parameter extraction
- [ ] Authentication framework
- [ ] Authorization helpers
- [ ] Integration with conditional headers (If-Match, etc.)

### Phase 6: Webmachine Decision Tree & Fusion
- [ ] `Resource` record type with all decision points
- [ ] `defaultResource` with sensible defaults
- [ ] Decision tree execution engine
- [ ] Lifecycle hooks (before/after each decision)
- [ ] Full conditional request handling (RFC 7232)
- [ ] Content negotiation engine (RFC 7231)
- [ ] Decision tree tracing/visualization
- [ ] CPS-style `DecisionM` for fusion
- [ ] Rewrite rules for decision elimination
- [ ] `StaticResource` for compile-time specialization
- [ ] Stream fusion for response bodies
- [ ] Inspection testing verification

### Phase 7: Type-Level API & Record-Based Routes
- [ ] Type-level combinators (`:>`, `:<|>`, `Capture`, etc.)
- [ ] Server derivation via type classes
- [ ] Client derivation
- [ ] Record-based routes (`NamedRoutes`, `GenericMode`)
- [ ] `(:-`) operator and `RouteMode` type
- [ ] `genericServer` and `genericClient` derivation
- [ ] Nested route composition with records
- [ ] OpenAPI/Swagger generation
- [ ] Integration with TH for hybrid approach

### Phase 8: Testing & Middleware
- [ ] Route testing DSL
- [ ] Property-based testing for client/server round-trips
- [ ] HAI-level middleware (more efficient than WAI)
- [ ] Common middleware (logging, CORS, compression, rate limiting)
- [ ] Error handling refinement

### Phase 9: OpenTelemetry Integration
- [ ] HTTP server span support with all [semantic conventions](https://opentelemetry.io/docs/specs/semconv/http/http-spans/)
- [ ] `http.route` attribute from routing layer
- [ ] HTTP client span support
- [ ] W3C Trace Context propagation (`traceparent`, `tracestate`)
- [ ] Integration with Webmachine lifecycle hooks
- [ ] HTTP metrics (`http.server.request.duration`, etc.)
- [ ] `runServerWithOTel` convenience function
- [ ] Environment variable configuration (`OTEL_*`)

### Phase 10: Additional Backends
- [ ] HTTP/2 backend support
- [ ] QUIC/HTTP/3 backend (experimental)
- [ ] Unix socket backend
- [ ] In-memory backend for testing

### Phase 11: Documentation & Polish
- [ ] Tutorial documentation
- [ ] API reference
- [ ] Example applications (REST API, WebSocket, SSE)
- [ ] Performance benchmarks vs WAI/Servant/Scotty
- [ ] Migration guide from WAI

---

## Part 17: Example Application

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

-- | Main using HAI with multiple backend options
main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  -- Choose your backend:
  -- runWarp warpSettings routes           -- Production (WAI/Warp)
  -- runSocket socketSettings routes       -- Development
  -- runHTTP2 http2Settings routes         -- HTTP/2
  runWarp (Warp.setPort 8080 Warp.defaultSettings) routes

-- | Alternative: Using Webmachine-style resources
mainWebmachine :: IO ()
mainWebmachine = do
  let app = toHAIApplication $ path "users" $ capture >>= \uid ->
              resource (userResourceWebmachine uid)
  runWarp (Warp.setPort 8080 Warp.defaultSettings) app

-- | User resource with full HTTP semantics
userResourceWebmachine :: Int -> Resource Handler
userResourceWebmachine userId = defaultResource
  { resourceAllowedMethods = pure [mGet, mPut, mDelete]
  , resourceExists = isJust <$> Database.lookup userId
  , resourceContentTypesProvided = pure
      [ (mediaTypeJson, Json <$> Database.lookup userId)
      ]
  , resourceGenerateETag = fmap (etagFromHash . hash) <$> Database.lookup userId
  , resourceLastModified = fmap userModifiedAt <$> Database.lookup userId
  }
```

---

## Part 18: Advanced Memory Patterns

This part covers advanced memory management patterns that build on the core
arena infrastructure defined in Part 10. These patterns address specific
performance scenarios like NUMA awareness, OS thread affinity, and linear
types for additional compile-time guarantees.

**Prerequisites**: Part 10.1 (Core Arena Infrastructure) defines the basic
`Arena`, `Scoped`, `Copyable`, and `escape` abstractions that are used here.

### 18.1 Linear Types for Stricter Guarantees

While rank-2 types (ST-style regions) prevent data from escaping at runtime,
linear types (GHC 9.0+) can provide even stronger compile-time guarantees
by ensuring every piece of request data is explicitly consumed or copied:

```haskell
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE GADTs #-}

import qualified Data.Unrestricted.Linear as Linear

-- | Linear request monad - request data must be consumed or copied
-- Unlike plain RouteT, this enforces that all Scoped data is handled
newtype RouteL r e m a where
  RouteL :: (Request r %1 -> m (Request r, RouteResult e a)) %1 -> RouteL r e m a

-- | Linear field access - must use the result
getPathL :: Request r %1 -> (Scoped r ByteString, Request r)
getPathL req = (pathRaw (requestPath req), req)

-- | To persist data, you MUST copy - returns Ur (unrestricted)
persistL :: Copyable a => Scoped r a %1 -> RouteL r e IO (Ur a)
persistL scoped = RouteL $ \req -> do
  copied <- deepCopy (unscoped scoped)
  pure (req, Matched (Ur copied))

-- | Ur wrapper for values that have left the linear world
data Ur a where
  Ur :: a -> Ur a

-- | Example: Linear request handling
handleLinear :: RouteL r ServerError IO (Response r)
handleLinear = Linear.do
  req <- getRequest
  (path, req') <- pure $ getPathL req

  -- TYPE ERROR if we try to ignore path:
  -- pure $ ok "done"  -- Error: 'path' not consumed

  -- Must either use it in response or copy it:
  Ur pathCopy <- persistL path
  liftIO $ Database.store pathCopy

  -- Now we can return
  finalizeRequest req'
  pure $ ok "done"
```

### 18.2 OS Thread Affinity and Worker Pools

GHC's green threads multiplex onto OS threads, making true TLS impossible.
However, we can design around this:

```haskell
-- | Capability-pinned worker with dedicated arena
data PinnedWorker = PinnedWorker
  { workerCapability :: !Int          -- GHC capability (OS thread)
  , workerArena      :: !(Ptr Word8)  -- Pre-allocated arena
  , workerArenaSize  :: !Int
  , workerArenaRef   :: !(IORef Int)  -- Current offset
  , workerId         :: !Int
  }

-- | Worker pool with OS thread affinity
data WorkerPool = WorkerPool
  { poolWorkers :: !(Vector PinnedWorker)
  , poolSize    :: !Int
  }

-- | Create a worker pool with one worker per capability
createWorkerPool :: Int      -- ^ Arena size per worker
                 -> IO WorkerPool
createWorkerPool arenaSize = do
  numCaps <- getNumCapabilities
  workers <- V.generateM numCaps $ \cap -> do
    arena <- mallocBytes arenaSize
    ref <- newIORef 0
    pure $ PinnedWorker cap arena arenaSize ref cap
  pure $ WorkerPool workers numCaps

-- | Run an action pinned to a specific OS thread's arena
-- Uses forkOn to ensure capability affinity
withPinnedArena :: WorkerPool
                -> Int  -- ^ Worker index
                -> (forall r. RequestT r IO a)
                -> IO a
withPinnedArena pool idx action = do
  let worker = poolWorkers pool V.! (idx `mod` poolSize pool)

  -- Reset arena for this request
  writeIORef (workerArenaRef worker) 0

  -- Run pinned to specific capability
  resultVar <- newEmptyMVar
  _ <- forkOn (workerCapability worker) $ do
    let arena = RequestArena
          (workerArena worker)
          (workerArenaSize worker)
          (workerArenaRef worker)
    result <- runReaderT (unRequestT action) arena
    putMVar resultVar result

  takeMVar resultVar

-- | Round-robin request distribution
data RequestDispatcher = RequestDispatcher
  { dispatcherPool    :: !WorkerPool
  , dispatcherCounter :: !(IORef Int)
  }

dispatchRequest :: RequestDispatcher
                -> (forall r. RequestT r IO Response)
                -> IO Response
dispatchRequest dispatcher action = do
  idx <- atomicModifyIORef' (dispatcherCounter dispatcher) $ \n ->
    (n + 1, n)
  withPinnedArena (dispatcherPool dispatcher) idx action
```

### 18.3 Bound Threads for True TLS

When you truly need thread-local storage (e.g., for C libraries):

```haskell
-- | Worker with bound OS thread (forkOS)
data BoundWorker = BoundWorker
  { boundThread    :: !ThreadId
  , boundArena     :: !(Ptr Word8)
  , boundArenaSize :: !Int
  , boundWorkQueue :: !(TBQueue WorkItem)
  , boundTLS       :: !(Ptr TLSData)  -- True thread-local storage
  }

data WorkItem = WorkItem
  { workAction :: IO ()
  , workResult :: MVar (Either SomeException ())
  }

-- | Thread-local storage structure (C-compatible)
data TLSData = TLSData
  { tlsArenaPtr    :: !(Ptr Word8)
  , tlsArenaOffset :: !Int
  , tlsConnectionPool :: !(Ptr ())  -- Connection pool for this thread
  , tlsRNG         :: !(Ptr ())     -- Thread-local RNG state
  }

-- | Create a bound worker with true OS thread
createBoundWorker :: Int -> IO BoundWorker
createBoundWorker arenaSize = do
  arena <- mallocBytes arenaSize
  queue <- newTBQueueIO 1024
  tlsPtr <- mallocBytes (sizeOf (undefined :: TLSData))
  poke tlsPtr $ TLSData arena 0 nullPtr nullPtr

  -- forkOS creates an actual OS thread
  tid <- forkOS $ forever $ do
    WorkItem action resultVar <- atomically $ readTBQueue queue
    result <- try action
    putMVar resultVar result

  pure $ BoundWorker tid arena arenaSize queue tlsPtr

-- | Submit work to bound thread
submitToBoundWorker :: BoundWorker -> IO a -> IO a
submitToBoundWorker worker action = do
  resultVar <- newEmptyMVar
  atomically $ writeTBQueue (boundWorkQueue worker) $
    WorkItem (action >>= putMVar resultVar) resultVar
  result <- takeMVar resultVar
  either throwIO pure =<< takeMVar resultVar
```

### 18.4 Leveraging GHC's NUMA Support

GHC's RTS has built-in NUMA support. When enabled, it automatically:
- Pins capabilities to NUMA nodes
- Allocates nursery and heap on NUMA-local memory
- Schedules threads to maintain locality

```haskell
-- Run with NUMA enabled:
-- myapp +RTS --numa -N8

-- Our worker pool just needs to pin to capabilities;
-- the RTS handles NUMA-local allocation automatically
data WorkerPool = WorkerPool
  { poolWorkers :: !(Vector PinnedWorker)
  , poolSize    :: !Int
  }

-- | Create worker pool - one per capability
-- Memory allocated by each worker will be NUMA-local
-- when running with +RTS --numa
createWorkerPool :: Int -> IO WorkerPool
createWorkerPool arenaSize = do
  numCaps <- getNumCapabilities
  workers <- V.generateM numCaps $ \cap -> do
    -- malloc here will be NUMA-local when running on that capability
    -- due to RTS NUMA support
    arena <- mallocBytes arenaSize
    ref <- newIORef 0
    pure $ PinnedWorker cap arena arenaSize ref cap
  pure $ WorkerPool workers numCaps

-- | Run on specific capability - RTS ensures NUMA locality
withPinnedArena :: WorkerPool -> Int -> (forall r. Arena r -> IO a) -> IO a
withPinnedArena pool idx action = do
  let worker = poolWorkers pool V.! (idx `mod` poolSize pool)
  writeIORef (workerArenaRef worker) 0  -- Reset arena

  -- forkOn pins to capability; RTS NUMA support handles memory locality
  resultVar <- newEmptyMVar
  _ <- forkOn (workerCapability worker) $ do
    result <- action (Arena (workerArena worker) (workerArenaSize worker) (workerArenaRef worker))
    putMVar resultVar result
  takeMVar resultVar

-- For explicit NUMA queries (rarely needed):
-- Use GHC.RTS.Flags to check if NUMA is enabled
-- Use GHC.Conc to query capability count
```

**Deployment notes:**
- Enable with `+RTS --numa` or `+RTS --numa=<nodes>`
- Combine with `-N` to set capability count
- The RTS queries `/sys/devices/system/node/` on Linux
- Works automatically - no application code changes needed

### 18.5 Hybrid Worker Pool (Pinned + Bound)

See Part 10.5 for basic arena integration. This section covers hybrid
pooling when some requests need bound threads for C library TLS.

```haskell
-- | Hybrid pool: pinned workers for speed, bound workers for TLS
data HybridPool = HybridPool
  { hpPinnedPool  :: !WorkerPool           -- Fast path: forkOn workers
  , hpBoundPool   :: !(Vector BoundWorker) -- Slow path: forkOS for TLS
  , hpDispatcher  :: !(IORef Int)          -- Round-robin counter
  }

-- | Dispatch based on TLS requirement
-- Most requests use fast pinned workers; some need bound threads
-- for C libraries that use thread-local storage (OpenSSL, some DB drivers)
dispatchHybrid :: HybridPool
               -> Bool  -- ^ Does this request need TLS?
               -> (forall r. RouteT r e IO a)
               -> IO a
dispatchHybrid pool needsTLS action = do
  idx <- atomicModifyIORef' (hpDispatcher pool) $ \n -> (n + 1, n)

  if needsTLS
    then do
      -- Route to bound thread for TLS support
      let worker = hpBoundPool pool V.! (idx `mod` V.length (hpBoundPool pool))
      submitToBoundWorker worker $ withLocalArena action
    else do
      -- Route to pinned worker (fast path)
      withPinnedArena (hpPinnedPool pool) idx $ \arena ->
        runRouteT' arena action

-- | Mark routes that need TLS (e.g., calling OpenSSL directly)
class HasTLSRequirement a where
  needsTLS :: a -> Bool

-- | Default: most routes don't need TLS
instance HasTLSRequirement (RouteT r e m a) where
  needsTLS _ = False

-- | Resources can declare TLS requirement
instance HasTLSRequirement (Resource r m) where
  needsTLS = resourceNeedsTLS  -- Optional field, defaults to False
```

### 18.6 Chunked Arena for Large Requests

For requests that exceed initial arena size:

```haskell
-- | Chunked arena that can grow
data ChunkedArena r = ChunkedArena
  { caChunks   :: !(IORef [ArenaChunk])
  , caCurrentChunk :: !(IORef ArenaChunk)
  , caChunkSize :: !Int
  }

data ArenaChunk = ArenaChunk
  { chunkPtr    :: !(Ptr Word8)
  , chunkSize   :: !Int
  , chunkOffset :: !(IORef Int)
  }

-- | Allocate in chunked arena, growing if necessary
chunkedAlloc :: ChunkedArena r -> Int -> IO (Ptr Word8)
chunkedAlloc ca size = do
  current <- readIORef (caCurrentChunk ca)
  offset <- readIORef (chunkOffset current)

  if offset + size <= chunkSize current
    then do
      -- Fits in current chunk
      writeIORef (chunkOffset current) (offset + size)
      pure $ chunkPtr current `plusPtr` offset
    else do
      -- Need new chunk
      let newSize = max (caChunkSize ca) size
      newPtr <- mallocBytes newSize
      newOffsetRef <- newIORef size
      let newChunk = ArenaChunk newPtr newSize newOffsetRef

      -- Add to chunk list (for cleanup)
      modifyIORef' (caChunks ca) (current :)
      writeIORef (caCurrentChunk ca) newChunk

      pure newPtr

-- | Free all chunks
freeChunkedArena :: ChunkedArena r -> IO ()
freeChunkedArena ca = do
  chunks <- readIORef (caChunks ca)
  current <- readIORef (caCurrentChunk ca)
  mapM_ (free . chunkPtr) (current : chunks)
```

### 18.7 Zero-Copy Request Parsing with Arenas

Combine arena allocation with zero-copy parsing:

```haskell
-- | Parse HTTP request with zero-copy into arena
parseHTTPRequest :: ChunkedArena r
                 -> ByteString  -- ^ Raw input (pinned memory from socket)
                 -> IO (Request r)
parseHTTPRequest arena input = do
  -- Parser that records slices instead of copying
  runFlatParse input $ do
    method <- parseMethod  -- Small, copied
    _ <- skipSpace

    -- Path: record slice into original buffer
    pathStart <- getOffset
    pathEnd <- skipWhile (/= ' ')
    pathLen <- subtract pathStart <$> getOffset

    let pathSlice = PS (unsafeCoerce# input) pathStart pathLen

    -- Store slice reference in arena
    pathRef <- liftIO $ chunkedAlloc arena (sizeOf (undefined :: ByteString))
    liftIO $ poke (castPtr pathRef) pathSlice

    -- Headers: parse into arena-allocated vector
    headers <- parseHeadersToArena arena

    pure $ Request
      { reqPath' = Scoped pathSlice
      , reqMethod' = method
      , reqHeaders' = Scoped headers
      , reqBody' = undefined  -- Parsed lazily
      }
```

### 18.8 Safety Properties and Verification

The arena system provides these safety guarantees:

```haskell
-- | Properties we can verify:

-- 1. No escape without copy (rank-2 type ensures this)
-- COMPILE ERROR:
badHandler :: IO ByteString
badHandler = withRequestArena 4096 $ do
  Scoped bs <- arenaByteString "hello"
  pure bs  -- Error: 'r' would escape its scope

-- CORRECT:
goodHandler :: IO ByteString
goodHandler = withRequestArena 4096 $ do
  Scoped bs <- arenaByteString "hello"
  escape (Scoped bs)  -- Explicit copy

-- 2. Linear types ensure consumption (with LinearTypes)
-- COMPILE ERROR:
badLinear :: RequestL r IO ()
badLinear = Linear.do
  (path, req) <- getPath <$> getRequest
  pure ()  -- Error: 'path' is not consumed

-- CORRECT:
goodLinear :: RequestL r IO ()
goodLinear = Linear.do
  (path, req) <- getPath <$> getRequest
  _ <- persistPath path  -- path is consumed
  finalizeRequest req    -- req is consumed
  pure ()

-- 3. Inspection testing for arena optimizations
{-# LANGUAGE TemplateHaskell #-}

-- Verify that arena operations are inlined
inspect $ 'handleRequest `hasNoType` ''IORef
inspect $ 'handleRequest `hasNoAllocation` ''ByteString
```

---

## Appendix A: Comparison Matrix

| Feature | Akka HTTP | Servant | Webmachine | Hermes (Proposed) |
|---------|-----------|---------|------------|-------------------|
| Route Definition | Runtime DSL | Type-level | Resource callbacks | All three + Records |
| Type Safety | Moderate | Maximum | Low | High |
| Error Messages | Good | Complex | Good | Good (TH-enhanced) |
| Client Generation | Manual | Automatic | N/A | Automatic |
| Server Generation | Manual | Automatic | N/A | Automatic |
| OpenAPI/Swagger | Plugin | Built-in | N/A | Planned |
| Performance | Excellent | Excellent | Excellent | Excellent (goal) |
| Learning Curve | Moderate | Steep | Moderate | Moderate (goal) |
| Memory Management | JVM GC | GHC GC | Erlang GC | Arena + Region types |
| Request Data Scope | Manual | Manual | Process-scoped | Region-scoped |
| HTTP Semantics | Manual | Manual | Built-in | Built-in (Resource) |
| Conditional Requests | Manual | Manual | Built-in | Built-in |
| Content Negotiation | Basic | Basic | Full | Full |
| Backend Abstraction | Akka Streams | WAI only | Cowboy | HAI (multiple) |
| Header Type Safety | Limited | Good | None | Excellent |
| Template Haskell | No | No | N/A | Yes |
| Decision Tree | No | No | Yes | Yes |
| Record-Based Routes | No | Yes (NamedRoutes) | No | Yes |
| Route Overlap Detection | No | Type errors | N/A | TH + Runtime |
| OpenTelemetry | Manual | Manual | Manual | Built-in |

---

## Appendix B: Key Dependencies

```yaml
dependencies:
  # Existing Hermes deps (core)
  - flatparse        # Efficient binary parsing
  - mason            # Fast ByteString builder
  - symbolize        # String interning for O(1) comparison

  # HAI backends (pick what you need)
  - wai >= 3.2.3     # WAI adapter (for Warp compatibility)
  - warp             # Production HTTP server (via WAI)
  - network          # Raw socket backend

  # Routing & serialization
  - aeson            # JSON support
  - http-media       # Content negotiation
  - http-api-data    # Path/query parsing

  # Monad & async
  - mtl              # Monad transformers
  - unliftio         # Async support
  - resourcet        # Resource management

  # Template Haskell
  - template-haskell # TH for route generation
  - th-lift          # TH lifting utilities

  # Performance & optimization verification
  - inspection-testing # Verify rewrite rules fire
  - primitive          # Low-level memory operations
  - compact            # Compact regions (GHC 8.2+)

  # Arena & linear types (optional advanced features)
  - linear-base        # Linear types support (GHC 9.0+)

  # OpenTelemetry
  - hs-opentelemetry-api          # OTel API
  - hs-opentelemetry-sdk          # OTel SDK
  - hs-opentelemetry-propagator-w3c # W3C Trace Context

  # Optional future backends
  - http2            # HTTP/2 support (future)
  - quic             # QUIC/HTTP3 (future, experimental)
```

---

## Appendix C: Module Structure

```
hermes/
├── src/
│   ├── Hermes/
│   │   ├── HAI.hs                    -- Hermes Application Interface
│   │   ├── HAI/
│   │   │   ├── Request.hs            -- High-performance request type
│   │   │   ├── Response.hs           -- Type-safe response type
│   │   │   ├── Backend.hs            -- Backend type class
│   │   │   └── Backend/
│   │   │       ├── WAI.hs            -- WAI adapter
│   │   │       ├── Socket.hs         -- Raw socket backend
│   │   │       └── HTTP2.hs          -- HTTP/2 backend
│   │   │
│   │   ├── Routing.hs                -- Route DSL
│   │   ├── Routing/
│   │   │   ├── Monad.hs              -- RouteT monad
│   │   │   ├── Path.hs               -- Path matching
│   │   │   ├── Method.hs             -- Method directives
│   │   │   ├── Header.hs             -- Header extraction
│   │   │   ├── Body.hs               -- Body handling
│   │   │   ├── Query.hs              -- Query parameters
│   │   │   ├── Analysis.hs           -- Route overlap detection
│   │   │   ├── Trie.hs               -- Route trie for matching
│   │   │   └── TH.hs                 -- Template Haskell support
│   │   │
│   │   ├── Resource.hs               -- Webmachine-style resources
│   │   ├── Resource/
│   │   │   ├── Decision.hs           -- Decision tree
│   │   │   ├── Hooks.hs              -- Lifecycle hooks
│   │   │   ├── Conditional.hs        -- Conditional requests
│   │   │   └── Negotiation.hs        -- Content negotiation
│   │   │
│   │   ├── API.hs                    -- Type-level API
│   │   ├── API/
│   │   │   ├── Combinators.hs        -- :>, :<|>, etc.
│   │   │   ├── Server.hs             -- Server derivation
│   │   │   ├── Client.hs             -- Client derivation
│   │   │   ├── Generic.hs            -- Record-based routes (NamedRoutes)
│   │   │   └── OpenAPI.hs            -- OpenAPI generation
│   │   │
│   │   ├── Telemetry.hs              -- OpenTelemetry integration
│   │   ├── Telemetry/
│   │   │   ├── Tracing.hs            -- HTTP server/client spans
│   │   │   ├── Metrics.hs            -- HTTP metrics
│   │   │   ├── Propagation.hs        -- W3C Trace Context
│   │   │   └── Attributes.hs         -- OTel semantic conventions
│   │   │
│   │   └── Test.hs                   -- Testing utilities
│   │
│   └── Network/HTTP/                 -- Existing Hermes modules
│       ├── Headers.hs
│       ├── Headers/...
│       ├── Methods.hs
│       ├── Status.hs
│       └── ...
```

---

## References

### Frameworks & Libraries
- [Akka HTTP Routing DSL](https://doc.akka.io/docs/akka-http/current/routing-dsl/overview.html)
- [Akka HTTP Directives](https://doc.akka.io/docs/akka-http/current/routing-dsl/directives/index.html)
- [Servant Documentation](https://www.servant.dev/)
- [Servant NamedRoutes](https://www.tweag.io/blog/2022-02-24-named-routes/) - Record-based API definition
- [Servant.API.Generic](https://hackage.haskell.org/package/servant/docs/Servant-API-Generic.html)
- [Type-level Web APIs with Servant (Paper)](https://www.andres-loeh.de/Servant/servant-wgp.pdf)
- [Webmachine](https://github.com/webmachine/webmachine) - Erlang HTTP semantic framework
- [Webmachine Decision Diagram](https://raw.githubusercontent.com/webmachine/webmachine/develop/docs/http-headers-status-v3.png)
- [WAI Interface](https://hackage.haskell.org/package/wai)

### OpenTelemetry
- [OpenTelemetry](https://opentelemetry.io/) - Observability framework
- [OTel HTTP Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/http/http-spans/) - HTTP span attributes
- [OTel HTTP Metrics Conventions](https://opentelemetry.io/docs/specs/semconv/http/http-metrics/) - HTTP metrics
- [W3C Trace Context](https://www.w3.org/TR/trace-context/) - Distributed tracing propagation
- [hs-opentelemetry](https://hackage.haskell.org/package/hs-opentelemetry-api) - Haskell OTel bindings

### HTTP RFCs
- [RFC 9110 - HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110)
- [RFC 7232 - Conditional Requests](https://datatracker.ietf.org/doc/html/rfc7232)
- [RFC 7231 - HTTP/1.1 Semantics and Content](https://datatracker.ietf.org/doc/html/rfc7231)
