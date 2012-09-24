{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CVector.Generic.MutableInternal
  ( CMVector(..)
  , capacity
  , popBack
  , popBackOverwrite
  , pushBack
  , fromVector
  , toVector

  , module Data.Vector.Generic.Mutable
  ) where

import Control.DeepSeq ( NFData, rnf )

import Control.Monad ( liftM )
import Control.Monad.Primitive ( PrimMonad, PrimState )

-- Import everything so that we can reexport it.
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Generic.Mutable as MVector


data CMVector v s a = CMVector {-# UNPACK #-} !Int !(v s a)

fromVector :: (MVector v a) => v s a -> CMVector v s a
fromVector mv = CMVector (MVector.length mv) mv
{-# INLINE fromVector #-}

toVector :: (MVector v a) => CMVector v s a -> v s a
toVector (CMVector l mv) = slice_ 0 l mv
{-# INLINE toVector #-}

instance (MVector v a) => MVector (CMVector v) a where
  basicLength (CMVector l _) = l
  {-# INLINE basicLength #-}

  basicUnsafeSlice from len (CMVector _ mv) =
    CMVector len (MVector.unsafeSlice from len mv)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps (CMVector _ mv1) (CMVector _ mv2) = MVector.overlaps mv1 mv2
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew i = liftM (CMVector i) (MVector.unsafeNew i)
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeReplicate i a = liftM (CMVector i) (MVector.replicate i a)
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeRead (CMVector _ mv) = MVector.unsafeRead mv
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (CMVector _ mv) = MVector.unsafeWrite mv
  {-# INLINE basicUnsafeWrite #-}

  basicClear (CMVector _ mv) = MVector.clear mv
  {-# INLINE basicClear #-}

  basicSet (CMVector l mv) = MVector.set (MVector.unsafeSlice 0 l mv)
  {-# INLINE basicSet #-}

  basicUnsafeCopy (CMVector _ mv1) (CMVector _ mv2) = MVector.unsafeCopy mv1 mv2
  {-# INLINE basicUnsafeCopy #-}

  basicUnsafeMove (CMVector _ mv1) (CMVector _ mv2) = MVector.unsafeMove mv1 mv2
  {-# INLINE basicUnsafeMove #-}

  basicUnsafeGrow (CMVector l mv) i
    | l' <= MVector.length mv = liftM (CMVector l') (MVector.clone mv)
    | otherwise               = do
        mv' <-MVector.unsafeGrow mv i
        MVector.unsafeCopy (MVector.unsafeSlice 0 l mv')
                           (MVector.unsafeSlice 0 l mv)
        return $ CMVector l' mv'
    where
      l' = l + i
  {-# INLINE basicUnsafeGrow #-}

instance (NFData (v s a)) => NFData (CMVector v s a) where
    rnf (CMVector _ v) = rnf v

capacity :: (MVector v a) => CMVector v s a -> Int
capacity (CMVector _ mv) = MVector.length mv
{-# INLINE capacity #-}

-- | Push an element at the back of the CVector.  If the size of the CVector and
-- its length are equal (i.e., it's full), the underlying Vector will be doubled
-- in size.  Otherwise no allocation will be done and the underlying vector will
-- be shared between the argument and the result.  /amortized O(1)/
pushBack :: (MVector v a, PrimMonad m)
  => CMVector v (PrimState m) a -> a -> m (CMVector v (PrimState m) a)
pushBack (CMVector l mv) a
  | MVector.length mv == 0 = do
      mv' <- MVector.new 1
      write_ mv' 0 a
      return (CMVector 1 mv')
  | l < MVector.length mv = do
      write_ mv l a
      return (CMVector (l + 1) mv)
  | otherwise = do
      mv' <- grow_ mv l
      write_ mv' l a
      return $ CMVector (l + 1) mv'

-- | Overwrite and remove an element from the back of the CVector.  Overwriting
-- with undefined value allows the GC to collect the popped element.  Must not
-- be used with unboxed 'CVector's.  Calls 'error' if the CVector is empty.
-- Does not shrink the underlying Vector.  /O(1)/
popBackOverwrite :: (MVector v a, PrimMonad m)
  => CMVector v (PrimState m) a -> m (CMVector v (PrimState m) a)
popBackOverwrite (CMVector l mv)
  | l == 0    = cannotPop
  | otherwise = write_ mv (l - 1) poppedElem >> return (CMVector (l - 1) mv)

-- | Remove an element from the back of the CVector.  Calls 'error' if the
-- CVector is empty.  Can be used with unboxed 'CVector's.  Does not shrink the
-- underlying Vector.  /O(1)/
popBack :: (MVector v a, PrimMonad m)
  => CMVector v (PrimState m) a -> m (CMVector v (PrimState m) a)
popBack (CMVector l mv)
  | l == 0    = cannotPop
  | otherwise = return (CMVector (l - 1) mv)

cannotPop :: a
cannotPop =
  error "Data.CVector.Generic.Mutable: cannot popBack from an empty vector"

poppedElem :: a
poppedElem = error "Data.CVector.Generic.Mutable: popped element"

grow_ :: (MVector v a, PrimMonad m)
  => v (PrimState m) a -> Int -> m (v (PrimState m) a)
#ifdef BOUNDS_CHECKING
grow_ = MVector.grow
#else
grow_ = MVector.unsafeGrow
#endif

slice_ :: (MVector v a) => Int -> Int -> v s a -> v s a
#ifdef BOUNDS_CHECKING
slice_ = MVector.slice
#else
slice_ = MVector.unsafeSlice
#endif

write_ :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> a -> m ()
#ifdef BOUNDS_CHECKING
write_ = MVector.write
#else
write_ = MVector.unsafeWrite
#endif
