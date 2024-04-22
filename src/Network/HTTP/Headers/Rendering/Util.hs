module Network.HTTP.Headers.Rendering.Util where

import qualified Data.Foldable1 as F1
import qualified Mason.Builder as M
import Data.Text.Short (ShortText, toShortByteString)

sepByCommas1 :: (F1.Foldable1 t, M.Buildable s) => t (M.BuilderFor s) -> (M.BuilderFor s)
sepByCommas1 = F1.intercalate1 ", "
{-# INLINE sepByCommas1 #-}

shortText :: ShortText -> M.Builder
shortText = M.shortByteString . toShortByteString
{-# INLINE shortText #-}