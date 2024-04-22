{-# LANGUAGE StandaloneDeriving #-}
module Network.HTTP.Methods where

import Data.Array.Byte.Hash
import Data.Hashable (Hashable(..))
import Data.Interned
import Data.String
import Data.ByteString.Short (ShortByteString, toShort)
import System.IO.Unsafe (unsafePerformIO)


data Method = Method
  { methodHash :: {-# UNPACK #-} !SipHash 
  , methodName :: {-# UNPACK #-} !ShortByteString
  }

instance Eq Method where
  Method h1 _ == Method h2 _ = h1 == h2

instance Ord Method where
  compare (Method h1 _) (Method h2 _) = compare h1 h2

instance Interned Method where
  data Description Method = MethodDescription !SipHash
  type Uninterned Method = ShortByteString
  describe = MethodDescription . sipHash 1 3 unstableHashKey
  cache = methodCache
  cacheWidth _ = 64

deriving instance Eq (Description Method)
deriving instance Ord (Description Method)
instance Hashable (Description Method) where
  hash (MethodDescription (SipHash h)) = fromIntegral h
  hashWithSalt salt (MethodDescription (SipHash h)) = hashWithSalt salt (fromIntegral h :: Int)

instance IsString Method where
  fromString = intern . toShort . fromString

methodCache :: Cache Method
methodCache = mkCache
{-# NOINLINE methodCache #-}

mGet :: Method
mGet = "GET"

mPost :: Method
mPost = "POST"

mPut :: Method
mPut = "PUT"

mDelete :: Method
mDelete = "DELETE"

mHead :: Method
mHead = "HEAD"

mOptions :: Method
mOptions = "OPTIONS"

mTrace :: Method
mTrace = "TRACE"

mPatch :: Method
mPatch = "PATCH"

mConnect :: Method
mConnect = "CONNECT"

mACL :: Method
mACL = "ACL"

mBaselineControl :: Method
mBaselineControl = "BASELINE-CONTROL"

mBind :: Method
mBind = "BIND"

mCheckin :: Method
mCheckin = "CHECKIN"

mCheckout :: Method
mCheckout = "CHECKOUT"

mCopy :: Method
mCopy = "COPY"

mLabel :: Method
mLabel = "LABEL"

mLink :: Method
mLink = "LINK"

mLock :: Method
mLock = "LOCK"

mMerge :: Method
mMerge = "MERGE"

mMkActivity :: Method
mMkActivity = "MKACTIVITY"

mMkCalendar :: Method
mMkCalendar = "MKCALENDAR"

mMkCol :: Method
mMkCol = "MKCOL"

mMkRedirectRef :: Method
mMkRedirectRef = "MKREDIRECTREF"

mMkWorkspace :: Method
mMkWorkspace = "MKWORKSPACE"

mMove :: Method
mMove = "MOVE"

mOrderPatch :: Method
mOrderPatch = "ORDERPATCH"

mPropFind :: Method
mPropFind = "PROPFIND"

mPropPatch :: Method
mPropPatch = "PROPPATCH"

mRebind :: Method
mRebind = "REBIND"

mReport :: Method
mReport = "REPORT"

mSearch :: Method
mSearch = "SEARCH"

mUnbind :: Method
mUnbind = "UNBIND"

mUpdate :: Method
mUpdate = "UPDATE"

mUpdateDirectRef :: Method
mUpdateDirectRef = "UPDATEDIRECTREF"

mVersionControl :: Method
mVersionControl = "VERSION-CONTROL"


