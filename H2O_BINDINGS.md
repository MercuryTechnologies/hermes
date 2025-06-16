# H2O HTTP Server Bindings for Haskell

This document provides comprehensive documentation for the H2O HTTP server bindings in the Hermes project. These bindings provide high-performance, type-safe access to the libh2o HTTP server library from Haskell.

## Overview

H2O is a high-performance HTTP server implementation that supports HTTP/1.1, HTTP/2, and WebSocket protocols. These Haskell bindings provide both low-level FFI access and high-level, type-safe APIs for building HTTP servers.

## Architecture

The H2O bindings are organized into three main layers:

### 1. Low-Level Types (`Network.H2O.Types`)

Provides direct mappings to H2O C structures and constants:

- `H2OContext`, `H2OGlobalConf`, `H2OEvLoop` - Core H2O types
- `H2OIovec`, `H2OStringRef` - Data structures for efficient I/O
- HTTP status codes, methods, and socket flags
- Memory-safe string conversion utilities

### 2. Core Bindings (`Network.H2O.Core`)

Direct FFI bindings to libh2o functions:

- Configuration management (`h2oConfigInit`, `h2oConfigRegisterHost`)
- Context and event loop operations
- Socket and network I/O
- Request/response handling
- Memory pool management

### 3. High-Level API (`Network.H2O`)

Type-safe, Haskell-friendly interface:

- `H2OServer` - Server handle with automatic resource management
- `Handler` - Type-safe request handlers
- `Request`/`Response` - HTTP message representations
- Exception handling and error management

## Quick Start

### Basic Server Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.H2O

main :: IO ()
main = do
  result <- runH2OServer defaultConfig $ \server -> do
    registerHandler server "/" $ \req -> do
      return $ responseOk "Hello, World!"
    
    registerHandler server "/json" $ \req -> do
      return $ responseJson "{\"message\": \"Hello from H2O!\"}"
    
    bindAndListen server "127.0.0.1" 8080
  
  case result of
    Left err -> putStrLn $ "Server error: " ++ err
    Right () -> putStrLn "Server finished"
```

### Configuration Options

```haskell
let config = defaultConfig 
      { h2oConfigMaxRequestSize = 2 * 1024 * 1024  -- 2MB
      , h2oConfigTimeout = 60                      -- 60 seconds
      , h2oConfigKeepAlive = True
      , h2oConfigH2 = True                         -- Enable HTTP/2
      }
```

## Performance Characteristics

The H2O bindings are designed to meet strict performance requirements:

### Memory Usage
- **Baseline**: < 4KB per connection
- **HTTP/1.1**: 4-8KB per connection
- **HTTP/2**: 8-16KB per connection
- **HTTP/3**: 16-32KB per connection (future)

### Throughput Targets
- **HTTP/1.1**: > 100K requests/second
- **HTTP/2**: > 500K requests/second
- **Header processing**: > 1M headers/second
- **Latency**: < 1ms for local requests

### Memory Management
- Zero-copy I/O where possible
- Efficient memory pooling
- Automatic resource cleanup
- Configurable buffer sizes

## Safety Features

### Memory Safety
- Automatic resource management with `bracket`
- Foreign pointer management for C structures
- Bounds checking on memory operations
- Protection against use-after-free

### Exception Safety
- Custom exception types for H2O-specific errors
- Comprehensive error handling
- Resource cleanup on exceptions
- Type-safe error propagation

### Concurrency Safety
- Thread-safe operations where appropriate
- Proper synchronization for shared resources
- Safe concurrent handler registration

## Testing and Verification

### Property-Based Testing
The bindings include extensive property-based tests using Hedgehog:

```bash
cabal test hermes-test
```

Test categories:
- Memory allocation/deallocation cycles
- String conversion round-trips
- Configuration validation
- Concurrent access patterns
- Resource cleanup verification

### Benchmarking
Comprehensive performance benchmarks using Criterion:

```bash
cabal bench hermes-bench
```

Benchmark categories:
- Memory management operations
- String conversions and comparisons
- Handler registration and execution
- Concurrency and scalability
- Response building

### Segfault Prevention
Specific tests to prevent common FFI issues:
- Null pointer handling
- Buffer overflow protection
- Invalid memory access detection
- Resource leak prevention

## API Reference

### Core Types

#### `H2OServer`
Server handle providing safe access to H2O functionality.

```haskell
data H2OServer = H2OServer
  { h2oServerConfig     :: !H2OConfig
  , h2oServerGlobalConf :: !(Ptr H2OGlobalConf)
  , h2oServerContext    :: !(Ptr H2OContext)
  -- ... other fields
  }
```

#### `Request`
HTTP request representation.

```haskell
data Request = Request
  { requestMethod  :: !ByteString
  , requestPath    :: !ByteString
  , requestHeaders :: ![(ByteString, ByteString)]
  , requestBody    :: !ByteString
  }
```

#### `Response`
HTTP response representation.

```haskell
data Response = Response
  { responseStatus  :: !Int
  , responseHeaders :: ![(ByteString, ByteString)]
  , responseBody    :: !ByteString
  }
```

### Configuration

#### `H2OConfig`
Server configuration options.

```haskell
data H2OConfig = H2OConfig
  { h2oConfigMaxRequestSize :: !Int     -- Maximum request size
  , h2oConfigTimeout        :: !Int     -- Request timeout
  , h2oConfigKeepAlive      :: !Bool    -- Enable keep-alive
  , h2oConfigH2            :: !Bool    -- Enable HTTP/2
  }
```

#### `defaultConfig`
Default configuration with sensible defaults.

```haskell
defaultConfig :: H2OConfig
defaultConfig = H2OConfig
  { h2oConfigMaxRequestSize = 1024 * 1024  -- 1MB
  , h2oConfigTimeout = 30
  , h2oConfigKeepAlive = True
  , h2oConfigH2 = True
  }
```

### Server Management

#### `runH2OServer`
Run server with automatic resource management.

```haskell
runH2OServer :: H2OConfig -> (H2OServer -> IO a) -> IO (Either String a)
```

#### `bindAndListen`
Bind server to address and start accepting connections.

```haskell
bindAndListen :: H2OServer -> String -> Int -> IO ()
```

### Handler Registration

#### `Handler`
Type alias for request handlers.

```haskell
type Handler = Request -> IO Response
```

#### `registerHandler`
Register a handler for a specific path.

```haskell
registerHandler :: H2OServer -> ByteString -> Handler -> IO ()
```

### Response Builders

#### `responseOk`
Create a 200 OK response.

```haskell
responseOk :: ByteString -> Response
```

#### `responseJson`
Create a JSON response with appropriate content type.

```haskell
responseJson :: ByteString -> Response
```

#### `responseHtml`
Create an HTML response with appropriate content type.

```haskell
responseHtml :: ByteString -> Response
```

#### `responseNotFound`
Create a 404 Not Found response.

```haskell
responseNotFound :: Response
```

#### `responseError`
Create a 500 Internal Server Error response.

```haskell
responseError :: ByteString -> Response
```

## Advanced Usage

### Custom Error Handling

```haskell
import Control.Exception (catch)

main = do
  result <- runH2OServer defaultConfig $ \server -> do
    registerHandler server "/error" $ \req -> do
      -- Handler that might throw
      throwIO $ H2OHandlerError "Something went wrong"
    bindAndListen server "127.0.0.1" 8080
  `catch` \(ex :: H2OException) -> do
    putStrLn $ "H2O error: " ++ show ex
```

### Low-Level Access

For performance-critical applications, you can access the low-level bindings:

```haskell
import Network.H2O.Core
import Network.H2O.Types

-- Direct memory pool allocation
allocateFromPool :: Ptr H2OMemPool -> Int -> IO (Ptr Word8)
allocateFromPool pool size = h2oMemAllocPool pool (fromIntegral size)
```

### Custom String Handling

```haskell
import Network.H2O.Types

-- Zero-copy string operations
processString :: ByteString -> IO ByteString
processString bs = withH2OString bs $ \h2oStr -> do
  -- Process string in H2O format
  h2oStringToByteString h2oStr
```

## Integration with WAI

The H2O bindings can be integrated with the Web Application Interface (WAI):

```haskell
-- Future: WAI adapter for H2O
h2oRunWAI :: H2OConfig -> Application -> IO ()
```

## Performance Optimization Tips

### Memory Management
- Use memory pools for frequent allocations
- Prefer `withH2OString` for temporary strings
- Enable buffer recycling for high-throughput scenarios

### Request Handling
- Keep handlers pure when possible
- Use lazy ByteStrings for large responses
- Consider streaming for large data

### Concurrency
- Use STM for shared state
- Avoid blocking operations in handlers
- Consider using async for I/O operations

## Troubleshooting

### Common Issues

1. **Segmentation Faults**
   - Ensure proper resource cleanup
   - Check for null pointer access
   - Verify foreign pointer lifetime

2. **Memory Leaks**
   - Use memory pools correctly
   - Clean up resources in exception handlers
   - Monitor memory usage in production

3. **Performance Issues**
   - Profile with criterion benchmarks
   - Check for unnecessary allocations
   - Optimize hot paths

### Debugging

Enable detailed logging:

```haskell
import Debug.Trace

debugHandler :: Handler
debugHandler req = do
  traceIO $ "Processing: " ++ show (requestPath req)
  return $ responseOk "Debug response"
```

## Contributing

### Adding New Features
1. Add low-level FFI bindings in `Network.H2O.Core`
2. Add type definitions in `Network.H2O.Types`
3. Provide high-level interface in `Network.H2O`
4. Add comprehensive tests
5. Add performance benchmarks
6. Update documentation

### Testing Requirements
- Property-based tests for all public APIs
- Memory safety tests for FFI operations
- Performance regression tests
- Integration tests with real HTTP clients

### Performance Requirements
All new features must meet the performance targets:
- < 100ns per operation for hot paths
- Zero allocations where possible
- Sub-linear memory scaling

## License

The H2O bindings are licensed under the BSD-3-Clause license, same as the Hermes project.

## References

- [H2O HTTP Server](https://h2o.examp1e.net/)
- [libh2o Documentation](https://h2o.examp1e.net/configure.html)
- [Haskell FFI Guide](https://wiki.haskell.org/Foreign_Function_Interface)
- [Performance Guidelines](PROJECT_GUIDE.md)