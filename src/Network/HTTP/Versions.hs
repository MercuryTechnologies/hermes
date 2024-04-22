module Network.HTTP.Versions
  ( HTTPVersion
  , mkHTTPVersion
  , httpMajor
  , httpMinor
  , http0_9
  , http1_0
  , http1_1
  , http2_0
  , http3_0
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Hashable (Hashable)
import Data.Word (Word8)

newtype HTTPVersion = HTTPVersion { httpVersion :: Word8 }
  deriving stock (Eq)
  deriving newtype (Hashable)

instance Show HTTPVersion where
  showsPrec _ v = showString "mkHTTPVersion " . shows (httpMajor v) . showString " " . shows (httpMinor v)

instance Ord HTTPVersion where
  compare a b = case compare (httpMajor a) (httpMajor b) of
    EQ -> compare (httpMinor a) (httpMinor b)
    x -> x

mkHTTPVersion :: Word8 -> Word8 -> HTTPVersion
mkHTTPVersion major minor = HTTPVersion $ (major `shiftL` 4) .|. minor

httpMajor :: HTTPVersion -> Word8
httpMajor (HTTPVersion v) = v `shiftR` 4

httpMinor :: HTTPVersion -> Word8
httpMinor (HTTPVersion v) = v .&. 0x0F

http0_9, http1_0, http1_1, http2_0, http3_0 :: HTTPVersion
http0_9 = mkHTTPVersion 0 9
http1_0 = mkHTTPVersion 1 0
http1_1 = mkHTTPVersion 1 1
http2_0 = mkHTTPVersion 2 0
http3_0 = mkHTTPVersion 3 0
