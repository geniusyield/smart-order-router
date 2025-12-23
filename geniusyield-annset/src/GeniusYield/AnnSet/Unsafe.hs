{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | Unsafe pointer equality. Ported from the internals of `containers` (they don't export this)
module GeniusYield.AnnSet.Unsafe where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Unsafe.Coerce (unsafeCoerce)

{- | Checks if two pointers are equal. Yes means yes;
 no means maybe. The values should be forced to at least
 WHNF before comparison to get moderately reliable results.
-}
ptrEq :: a -> a -> Bool

{- | Checks if two pointers are equal, without requiring
 them to have the same type. The values should be forced
 to at least WHNF before comparison to get moderately
 reliable results.
-}
hetPtrEq :: a -> b -> Bool

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 707
ptrEq x y = reallyUnsafePtrEquality# x y ==# 1#
hetPtrEq x y = unsafeCoerce reallyUnsafePtrEquality# x y ==# 1#
#else
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
hetPtrEq x y = isTrue# (unsafeCoerce reallyUnsafePtrEquality# x y)
#endif

#else
-- Not GHC
ptrEq _ _ = False
hetPtrEq _ _ = False
#endif

{-# INLINE ptrEq #-}

{-# INLINE hetPtrEq #-}

infix 4 `ptrEq`

infix 4 `hetPtrEq`
