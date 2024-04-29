# Hermes

A Haskell library designed to provide a robust foundation for networking applications, with a current focus on HTTP header parsing and rendering. Named after the messenger of the gods in Greek mythology, Hermes aims to facilitate swift and reliable message handling across the internet.

## TODO

- `parseFromHeaders` should provide basic combinators to accept only one header or else combine headers where appropriate.
- `renderToHeaders` needs a Builder implementation that can emit a list of headers when a header becomes too long.
