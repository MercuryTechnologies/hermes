{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
-- Inspired by the siphash implementation in the 'memory' package.
--
-- This module provides a simple implementation of the SipHash algorithm.
-- It differs from the memory package in that it works on ByteArrays instead of
-- pointers to pinned memory, which means that it can be used with unpinned byte arrays.
--
-- reference: <http://131002.net/siphash/siphash.pdf>
--
module Data.Array.Byte.Hash where
import Control.Monad.Random.Lazy
import Data.Array.Byte (ByteArray(..))
import Data.ByteString.Short (ShortByteString(..))
import Data.Text.Short (ShortText, toShortByteString)
import Data.Text.Internal (Text(..))
import qualified Data.Text.Array as A
import Data.Memory.Endian
import Data.Word
import Data.Bits
import Data.Typeable (Typeable)
import GHC.Exts
import GHC.Int
import GHC.Word
import Control.Monad
import System.IO.Unsafe

unstableHashKey :: SipKey
unstableHashKey = unsafePerformIO $ evalRandIO (SipKey <$> getRandom <*> getRandom)
{-# NOINLINE unstableHashKey #-}

class SipHashable a where
  sipHash :: Int -> Int -> SipKey -> a -> SipHash
{-# SPECIALIZE sipHash :: Int -> Int -> SipKey -> Text -> SipHash #-}
{-# SPECIALIZE sipHash :: Int -> Int -> SipKey -> ShortText -> SipHash #-}
{-# SPECIALIZE sipHash :: Int -> Int -> SipKey -> ShortByteString -> SipHash #-}

instance SipHashable Text where
  sipHash c d key (Text (A.ByteArray arr) off len) = hashWith c d key (ByteArray arr) off len

instance SipHashable ShortText where
  sipHash c d key st = sipHash c d key (toShortByteString st)

instance SipHashable ShortByteString where
  sipHash c d key (SBS arr) = hashWith c d key (ByteArray arr) 0 (I# (sizeofByteArray# arr))

-- | SipHash Key
data SipKey = SipKey {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | SipHash tag value
newtype SipHash = SipHash Word64
    deriving (Show, Eq, Ord, Typeable)

data InternalState = InternalState {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

hashWith 
  :: Int -- ^ siphash C
  -> Int -- ^ siphash D
  -> SipKey -- ^ key for the hash
  -> ByteArray
  -> Int -- ^ offset
  -> Int -- ^ length
  -> SipHash
hashWith c d key arr@(ByteArray byteArr) off@(I# off#) totalLen = runHash (initSip key) off totalLen
  where 
    indexOff :: Int -> Int -> Word8
    indexOff !(I# curr#) !(I# n#) = W8# (indexWord8Array# byteArr (off# +# curr# +# n#))

    readAsWord64 :: Int -> Word64
    readAsWord64 !curr = 
      let !(I# i#) = off + curr
      in W64# (indexWord8ArrayAsWord64# byteArr i#)

    runHash :: InternalState -> Int -> Int -> SipHash
    runHash !st !ptr l
      | l > 7     = runHash (process st (fromLE $ LE $ readAsWord64 ptr)) (ptr + 8) (l - 8)
      | otherwise = let !lengthBlock = (fromIntegral totalLen `mod` 256) `unsafeShiftL` 56 in
          finish $ process st $ case l of
            0 -> lengthBlock :: Word64
            1 ->
              let v0 = indexOff ptr 0
              in lengthBlock .|. to64 v0
            2 -> 
              let v0 = indexOff ptr 0
                  v1 = indexOff ptr 1
              in lengthBlock
                  .|. (to64 v1 `unsafeShiftL` 8)
                  .|. to64 v0
            3 -> 
              let v0 = indexOff ptr 0
                  v1 = indexOff ptr 1
                  v2 = indexOff ptr 2
              in lengthBlock
                  .|. (to64 v2 `unsafeShiftL` 16)
                  .|. (to64 v1 `unsafeShiftL` 8)
                  .|. to64 v0
            4 -> 
              let v0 = indexOff ptr 0
                  v1 = indexOff ptr 1
                  v2 = indexOff ptr 2
                  v3 = indexOff ptr 3
              in lengthBlock
                  .|. (to64 v3 `unsafeShiftL` 24)
                  .|. (to64 v2 `unsafeShiftL` 16)
                  .|. (to64 v1 `unsafeShiftL` 8)
                  .|. to64 v0
            5 -> 
              let v0 = indexOff ptr 0
                  v1 = indexOff ptr 1
                  v2 = indexOff ptr 2
                  v3 = indexOff ptr 3
                  v4 = indexOff ptr 4
              in lengthBlock
                  .|. (to64 v4 `unsafeShiftL` 32)
                  .|. (to64 v3 `unsafeShiftL` 24)
                  .|. (to64 v2 `unsafeShiftL` 16)
                  .|. (to64 v1 `unsafeShiftL` 8)
                  .|. to64 v0
            6 -> 
              let v0 = indexOff ptr 0
                  v1 = indexOff ptr 1
                  v2 = indexOff ptr 2
                  v3 = indexOff ptr 3
                  v4 = indexOff ptr 4
                  v5 = indexOff ptr 5
              in lengthBlock
                  .|. (to64 v5 `unsafeShiftL` 40)
                  .|. (to64 v4 `unsafeShiftL` 32)
                  .|. (to64 v3 `unsafeShiftL` 24)
                  .|. (to64 v2 `unsafeShiftL` 16)
                  .|. (to64 v1 `unsafeShiftL` 8)
                  .|. to64 v0
            7 -> 
              let v0 = indexOff ptr 0
                  v1 = indexOff ptr 1
                  v2 = indexOff ptr 2
                  v3 = indexOff ptr 3
                  v4 = indexOff ptr 4
                  v5 = indexOff ptr 5
                  v6 = indexOff ptr 6
              in lengthBlock
                  .|. (to64 v6 `unsafeShiftL` 48)
                  .|. (to64 v5 `unsafeShiftL` 40)
                  .|. (to64 v4 `unsafeShiftL` 32)
                  .|. (to64 v3 `unsafeShiftL` 24)
                  .|. (to64 v2 `unsafeShiftL` 16)
                  .|. (to64 v1 `unsafeShiftL` 8)
                  .|. to64 v0
            _ -> error "siphash: internal error: cannot happens"

    {-# INLINE to64 #-}
    to64 :: Word8 -> Word64
    to64 = fromIntegral

    {-# INLINE process #-}
    process :: InternalState -> Word64 -> InternalState
    process istate m = newState
        where newState = postInject $! runRoundsCompression $! preInject istate
              preInject  (InternalState v0 v1 v2 v3) = InternalState v0 v1 v2 (v3 `xor` m)
              postInject (InternalState v0 v1 v2 v3) = InternalState (v0 `xor` m) v1 v2 v3

    {-# INLINE finish #-}
    finish :: InternalState -> SipHash
    finish istate = getDigest $! runRoundsDigest $! preInject istate
        where getDigest (InternalState v0 v1 v2 v3) = SipHash (v0 `xor` v1 `xor` v2 `xor` v3)
              preInject (InternalState v0 v1 v2 v3) = InternalState v0 v1 (v2 `xor` 0xff) v3

    {-# INLINE doRound #-}
    doRound :: InternalState -> InternalState
    doRound (InternalState v0 v1 v2 v3) =
          let !v0'    = v0 + v1
              !v2'    = v2 + v3
              !v1'    = v1 `rotateL` 13
              !v3'    = v3 `rotateL` 16
              !v1''   = v1' `xor` v0'
              !v3''   = v3' `xor` v2'
              !v0''   = v0' `rotateL` 32
              !v2''   = v2' + v1''
              !v0'''  = v0'' + v3''
              !v1'''  = v1'' `rotateL` 17
              !v3'''  = v3'' `rotateL` 21
              !v1'''' = v1''' `xor` v2''
              !v3'''' = v3''' `xor` v0'''
              !v2'''  = v2'' `rotateL` 32
            in InternalState v0''' v1'''' v2''' v3''''

    {-# INLINE runRoundsCompression #-}
    runRoundsCompression :: InternalState -> InternalState
    runRoundsCompression st
        | c == 2    = doRound $! doRound st
        | otherwise = loopRounds c st

    {-# INLINE runRoundsDigest #-}
    runRoundsDigest :: InternalState -> InternalState
    runRoundsDigest st
        | d == 3    = doRound $! doRound $! doRound st
        | d == 4    = doRound $! doRound $! doRound $! doRound st
        | otherwise = loopRounds d st

    {-# INLINE loopRounds #-}
    loopRounds :: Int -> InternalState -> InternalState
    loopRounds 1 !v = doRound v
    loopRounds n !v = loopRounds (n-1) (doRound v)

    {-# INLINE initSip #-}
    initSip :: SipKey -> InternalState
    initSip (SipKey k0 k1) = InternalState 
      (k0 `xor` 0x736f6d6570736575)
      (k1 `xor` 0x646f72616e646f6d)
      (k0 `xor` 0x6c7967656e657261)
      (k1 `xor` 0x7465646279746573)