module Foreign.Marshall.Mimalloc where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

foreign import ccall unsafe "mi_free" mi_free :: Ptr a -> IO ()

foreign import ccall unsafe "&mi_free" finalizer_mi_free :: FinalizerPtr a

foreign import ccall unsafe "mi_malloc" mi_malloc :: CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_zalloc" mi_zalloc :: CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_calloc" mi_calloc :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_realloc" mi_realloc :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall unsafe "mi_expand" mi_expand :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall unsafe "mi_mallocn" mi_mallocn :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_reallocn" mi_reallocn :: Ptr a -> CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_reallocf" mi_reallocf :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall unsafe "mi_malloc_small" mi_malloc_small :: CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_zalloc_small" mi_zalloc_small :: CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_malloc_aligned" mi_malloc_aligned :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_zalloc_aligned" mi_zalloc_aligned :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_calloc_aligned" mi_calloc_aligned :: CSize -> CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_realloc_aligned" mi_realloc_aligned :: Ptr a -> CSize -> CSize -> IO (Ptr b)

foreign import ccall unsafe "mi_malloc_aligned_at" mi_malloc_aligned_at :: CSize -> CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_zalloc_aligned_at" mi_zalloc_aligned_at :: CSize -> CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_calloc_aligned_at" mi_calloc_aligned_at :: CSize -> CSize -> CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "mi_realloc_aligned_at" mi_realloc_aligned_at :: Ptr a -> CSize -> CSize -> CSize -> CSize -> IO (Ptr b)
