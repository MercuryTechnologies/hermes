{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
The "HTTP Content Coding Registry", maintained by IANA at <https://www.iana.org/assignments/http-parameters/>, registers content-coding names.

These values are used in the "Content-Encoding" header field (see Section 8.4 of [RFC7231]) and the "Accept-Encoding" header field (see Section 5.3.4 of [RFC7231]) 
to indicate what content codings are supported by the sender and what codings were applied to the payload body.

Values to be added to this namespace require IETF Review (see Section 4.8 of [RFC8126]) and MUST conform to the purpose of content coding defined in Section 8.4.1.
-}
module Network.HTTP.ContentCoding where

import Data.Text.Short (ShortText)
import qualified Mason.Builder as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util

data ContentCoding
  = AES128GCM
  -- ^ AES-GCM encryption with a 128-bit content encryption key
  | Brotli
  -- ^ Brotli Compressed Data Format
  | Compress
  -- ^ UNIX "compress" data format [Welch, T., "A Technique for High Performance Data Compression", IEEE Computer 17(6), June 1984.]	
  | Deflate
  -- ^ "deflate" compressed data ([RFC1951]) inside the "zlib" data format ([RFC1950])	
  | EfficientXMLInterchange
  -- ^ W3C Efficient XML Interchange
  | GZip
  -- ^ GZIP file format [RFC1952]
  | Identity
  -- ^ The default (identity) encoding; the use of no transformation whatsoever
  | Pack200GZip
  -- ^ JAR file format with Pack200 compression, then GZIP compression
  | ZStd
  -- ^ A stream of bytes compressed using the Zstandard protocol	
  | Custom {-# UNPACK #-} !ShortText
  -- ^ A custom content coding. This constructor is used for content codings that are not part of the IANA registry.
  deriving stock (Eq, Show)

contentCodingParser :: ParserT st e ContentCoding
contentCodingParser = $(switch [| case _ of
  "aes128gcm" -> pure AES128GCM
  "br" -> pure Brotli
  "compress" -> pure Compress
  "deflate" -> pure Deflate
  "exi" -> pure EfficientXMLInterchange
  "gzip" -> pure GZip
  "identity" -> pure Identity
  "pack200-gzip" -> pure Pack200GZip
  "x-compress" -> pure Compress
  "x-gzip" -> pure GZip
  "zstd" -> pure ZStd
  _ -> Custom <$> rfc9110Token
  |])

renderContentCoding :: ContentCoding -> M.Builder
renderContentCoding = \case
  AES128GCM -> "aes128gcm"
  Brotli -> "br"
  Compress -> "compress"
  Deflate -> "deflate"
  EfficientXMLInterchange -> "exi"
  GZip -> "gzip"
  Identity -> "identity"
  Pack200GZip -> "pack200-gzip"
  ZStd -> "zstd"
  Custom x -> shortText x
