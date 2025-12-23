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

Hermes introduces **HAI** (Hermes Application Interface), a next-generation abstraction that:
- Uses Hermes's efficient `HeaderMap` with interned `HeaderFieldName`
- Supports multiple backends (WAI adapter, raw sockets, HTTP/2, QUIC)
- Provides zero-copy operations where possible
- Enables compile-time header direction checking

### 10.1 Core Types

```haskell
-- | The Hermes Application type - backend agnostic
type Application = Request -> (Response -> IO ResponseSent) -> IO ResponseSent

-- | Proof that response was sent (for type safety)
data ResponseSent = ResponseSent

-- | High-performance request representation
data Request = Request
  { -- Core request line
    requestMethod      :: {-# UNPACK #-} !Method           -- Interned method
  , requestPath        :: {-# UNPACK #-} !Path             -- Efficient path segments
  , requestQueryString :: !QueryString                     -- Parsed query params
  , requestHttpVersion :: {-# UNPACK #-} !HTTPVersion      -- Packed version

    -- Headers with Hermes types
  , requestHeaders     :: {-# UNPACK #-} !HeaderMap        -- Interned header names

    -- Body handling
  , requestBody        :: !RequestBody                     -- Streaming body
  , requestBodyLength  :: !RequestBodyLength               -- Known or chunked

    -- Connection info
  , requestRemoteHost  :: !SockAddr                        -- Client address
  , requestIsSecure    :: !Bool                            -- TLS?

    -- Raw access (for backends that need it)
  , requestRaw         :: !RawRequest                      -- Backend-specific
  }

-- | Efficient path representation using fusion
data Path = Path
  { pathSegments   :: {-# UNPACK #-} !(Vector Text)  -- Decoded segments
  , pathRaw        :: {-# UNPACK #-} !ByteString     -- Raw for forwarding
  , pathUnmatched  :: {-# UNPACK #-} !Int            -- Index of first unmatched
  }

-- | Pre-parsed query string
data QueryString = QueryString
  { queryParams  :: {-# UNPACK #-} !(HashMap Text (NonEmpty Text))
  , queryRaw     :: {-# UNPACK #-} !ByteString
  }

-- | Streaming request body
data RequestBody
  = KnownLengthBody {-# UNPACK #-} !Int64 !(IO ByteString)
  | ChunkedBody !(IO ByteString)
  | NoBody
```

### 10.2 Response Types

```haskell
-- | High-performance response
data Response = Response
  { responseStatus  :: {-# UNPACK #-} !StatusCode
  , responseHeaders :: {-# UNPACK #-} !HeaderMap      -- Type-safe headers!
  , responseBody    :: !ResponseBody
  }

-- | Response body variants
data ResponseBody
  = BuilderBody !Builder                              -- Efficient builder
  | StreamingBody !StreamingBody                      -- Streaming chunks
  | FileBody !FilePath !(Maybe FilePart)              -- Sendfile optimization
  | RawBody !(IO ByteString -> (ByteString -> IO ()) -> IO ())  -- Raw takeover

-- | Streaming body type
type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

-- | Smart constructors with type-safe headers
responseBuilder :: StatusCode -> HeaderMap -> Builder -> Response
responseBuilder status hdrs body = Response status hdrs (BuilderBody body)

responseStream :: StatusCode -> HeaderMap -> StreamingBody -> Response
responseStream status hdrs body = Response status hdrs (StreamingBody body)

responseFile :: StatusCode -> HeaderMap -> FilePath -> Maybe FilePart -> Response
responseFile status hdrs path part = Response status hdrs (FileBody path part)

-- | Type-safe header setting
setResponseHeader :: forall h.
  ( KnownHeader h
  , Direction h `AllowedIn` 'Response
  ) => h -> Response -> Response
setResponseHeader h resp = resp { responseHeaders = setHeader h (responseHeaders resp) }
```

### 10.3 Type-Safe Header Operations

```haskell
-- | Get a request header with compile-time direction check
getRequestHeader :: forall h.
  ( KnownHeader h
  , Direction h `AllowedIn` 'Request
  ) => Request -> Either (ParseFailure h) (Maybe h)
getRequestHeader req = lookupHeader @h (requestHeaders req)

-- | Set a response header with compile-time direction check
addResponseHeader :: forall h.
  ( KnownHeader h
  , Direction h `AllowedIn` 'Response
  ) => h -> HeaderMap -> HeaderMap
addResponseHeader = setHeader

-- | Compile-time error for wrong direction
-- This won't compile:
-- badExample :: Request -> Maybe SetCookie  -- SetCookie is Response-only!
-- badExample req = getRequestHeader @SetCookie req
-- Error: Header direction mismatch: SetCookie is Response, not Request

-- | Header presence witness
data HeaderPresent h = HeaderPresent
  { getHeaderValue :: h
  }

-- | Require a header (fails request if missing)
requireHeader :: forall h.
  ( KnownHeader h
  , Direction h `AllowedIn` 'Request
  ) => Request -> Either HeaderError (HeaderPresent h)
```

### 10.4 Backend Adapters

```haskell
-- | Type class for backend implementations
class Backend backend where
  type BackendConfig backend :: Type
  type BackendError backend :: Type

  -- | Run an application with this backend
  runBackend :: BackendConfig backend
             -> Application
             -> IO (Either (BackendError backend) ())

  -- | Convert backend-specific request to HAI Request
  toHAIRequest :: backend -> RawRequest -> IO Request

  -- | Convert HAI Response to backend-specific response
  fromHAIResponse :: backend -> Response -> IO RawResponse

-- | WAI Backend Adapter
data WAIBackend = WAIBackend

instance Backend WAIBackend where
  type BackendConfig WAIBackend = Warp.Settings
  type BackendError WAIBackend = SomeException

  runBackend settings app =
    try $ Warp.runSettings settings (toWaiApp app)

  toHAIRequest _ waiReq = do
    let headers = headerMapFromList (Wai.requestHeaders waiReq)
    pure Request
      { requestMethod = Method $ intern $ Wai.requestMethod waiReq
      , requestPath = pathFromWai waiReq
      , requestHeaders = headers
      , ...
      }

-- | Convert HAI app to WAI app
toWaiApp :: Application -> Wai.Application
toWaiApp haiApp waiReq waiRespond = do
  haiReq <- toHAIRequest WAIBackend waiReq
  haiApp haiReq $ \haiResp -> do
    waiResp <- fromHAIResponse WAIBackend haiResp
    waiRespond waiResp
    pure ResponseSent

-- | Raw Socket Backend (from existing SimpleServer)
data SocketBackend = SocketBackend

instance Backend SocketBackend where
  type BackendConfig SocketBackend = ServerSettings
  type BackendError SocketBackend = IOException

  runBackend settings app = runSocketServer settings app

-- | HTTP/2 Backend (future)
data HTTP2Backend = HTTP2Backend

instance Backend HTTP2Backend where
  type BackendConfig HTTP2Backend = HTTP2Settings
  type BackendError HTTP2Backend = HTTP2Error
  -- ...

-- | QUIC/HTTP3 Backend (future)
data QUICBackend = QUICBackend
```

### 10.5 Performance Optimizations

```haskell
-- | Zero-copy header access
-- Headers are stored with interned names, enabling O(1) comparison
lookupHeaderFast :: HeaderFieldName -> HeaderMap -> Maybe (NonEmpty ByteString)
lookupHeaderFast name (HeaderMap m) = Map.lookup name m  -- Symbol comparison is pointer equality

-- | Pre-computed common headers for responses
commonResponseHeaders :: HeaderMap
commonResponseHeaders = headerMapFromList
  [ (hServer, "Hermes")
  , (hConnection, "keep-alive")
  ]
{-# NOINLINE commonResponseHeaders #-}

-- | Efficient header map merging
mergeHeaders :: HeaderMap -> HeaderMap -> HeaderMap
mergeHeaders (HeaderMap a) (HeaderMap b) = HeaderMap (Map.unionWith (<>) a b)

-- | Builder-based response construction (no intermediate ByteStrings)
buildResponse :: Response -> Builder
buildResponse Response{..} = mconcat
  [ statusLineBuilder responseStatus
  , headersBuilder responseHeaders
  , crlfBuilder
  , case responseBody of
      BuilderBody b -> b
      _ -> mempty  -- Streaming handled separately
  ]

-- | Memory-mapped file responses
data FilePart = FilePart
  { filePartOffset :: {-# UNPACK #-} !Int64
  , filePartLength :: {-# UNPACK #-} !Int64
  }

-- | Sendfile support (when available)
responseFileSendfile :: StatusCode -> HeaderMap -> FilePath -> Maybe FilePart -> Response
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

### 12.2 Resource Definition

```haskell
-- | A Resource defines behavior at each HTTP decision point
data Resource m = Resource
  { -- Service availability
    resourceServiceAvailable    :: m Bool

    -- Method handling
  , resourceKnownMethods        :: [Method]
  , resourceAllowedMethods      :: m [Method]

    -- Authentication & Authorization
  , resourceIsAuthorized        :: m AuthResult
  , resourceForbidden           :: m Bool

    -- Content negotiation
  , resourceContentTypesProvided :: m [(MediaType, m ResponseBody)]
  , resourceContentTypesAccepted :: m [(MediaType, m ProcessResult)]
  , resourceLanguagesProvided    :: m (Maybe [LanguageTag])
  , resourceCharsetsProvided     :: m (Maybe [Charset])
  , resourceEncodingsProvided    :: m (Maybe [ContentCoding])

    -- Resource existence & lifecycle
  , resourceExists              :: m Bool
  , resourcePreviouslyExisted   :: m Bool
  , resourceMovedPermanently    :: m (Maybe URI)
  , resourceMovedTemporarily    :: m (Maybe URI)
  , resourceAllowMissingPost    :: m Bool
  , resourceDeleteResource      :: m Bool
  , resourceDeleteCompleted     :: m Bool
  , resourcePostIsCreate        :: m Bool
  , resourceCreatePath          :: m (Maybe Text)

    -- Conditional requests (ETags, Last-Modified)
  , resourceGenerateETag        :: m (Maybe ETag)
  , resourceLastModified        :: m (Maybe UTCTime)
  , resourceExpires             :: m (Maybe UTCTime)

    -- Caching
  , resourceOptions             :: m [Header]
  , resourceVariances           :: m [HeaderFieldName]

    -- Multiple representations
  , resourceMultipleChoices     :: m Bool
  }

-- | Default resource with sensible defaults
defaultResource :: Applicative m => Resource m
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

```haskell
-- | Execute the HTTP decision tree for a resource
runResource :: Monad m => Resource m -> Request -> m Response
runResource resource req = runDecisionTree decisions
  where
    decisions = DecisionTree
      { dtServiceAvailable = do
          available <- resourceServiceAvailable resource
          if available then Right <$> continue else pure $ Left status503

      , dtKnownMethod = do
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
data DecisionResult
  = Continue                         -- ^ Proceed to next decision
  | Respond !StatusCode ![Header]    -- ^ Short-circuit with response
  | Delegate !(m Response)           -- ^ Hand off to resource handler
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

### Phase 6: Webmachine Decision Tree
- [ ] `Resource` record type with all decision points
- [ ] `defaultResource` with sensible defaults
- [ ] Decision tree execution engine
- [ ] Lifecycle hooks (before/after each decision)
- [ ] Full conditional request handling (RFC 7232)
- [ ] Content negotiation engine (RFC 7231)
- [ ] Decision tree tracing/visualization

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
