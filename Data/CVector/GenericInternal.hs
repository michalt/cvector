{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.CVector.GenericInternal
  ( CVector(..)
  , freeze
  , capacity
  , thaw
  , fromVector
  , toVector

  -- Export everything from Vector.Generic except for freeze and thaw.
  , module Data.Vector.Generic
  ) where

import Prelude hiding ( (++), mapM_ )
import qualified Prelude

import Control.DeepSeq ( NFData, rnf )
import Control.Monad ( liftM )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Control.Monad.ST ( ST, runST )

import qualified Text.Read as Read


import Data.Monoid ( Monoid(..) )

import Data.Vector.Generic hiding ( freeze, readPrec, showsPrec, thaw )
import qualified Data.Vector.Generic as Vector

import Data.Vector.Generic.Mutable ( MVector )
import qualified Data.Vector.Generic.Mutable as MVector

import Data.CVector.Generic.MutableInternal ( CMVector(..) )

data CVector v a = CVector {-# UNPACK #-} !Int !(v a)

type instance Mutable (CVector v) = CMVector (Mutable v)

fromVector :: (Vector v a) => v a -> CVector v a
fromVector v = CVector (Vector.length v) v
{-# INLINE fromVector #-}

toVector :: CVector v a -> v a
toVector (CVector _ v) = v
{-# INLINE toVector #-}

instance (Vector v a) => Vector (CVector v) a where
  basicLength (CVector l _) = l
  {-# INLINE basicLength #-}

  basicUnsafeCopy (CMVector _ mv) (CVector _ v) = Vector.unsafeCopy mv v
  {-# INLINE basicUnsafeCopy #-}

  basicUnsafeFreeze (CMVector l mv) = liftM (CVector l) (Vector.unsafeFreeze mv)
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeIndexM (CVector _ v) = Vector.basicUnsafeIndexM v
  {-# INLINE basicUnsafeIndexM #-}

  basicUnsafeSlice from len (CVector l v) = CVector l (Vector.unsafeSlice from len v)
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeThaw (CVector l v) = liftM (CMVector l) (Vector.unsafeThaw v)
  {-# INLINE basicUnsafeThaw #-}

instance (Show (v a), Vector v a) => Show (CVector v a) where
  showsPrec = showsPrec_

instance (Read a, Vector v a) => Read (CVector v a) where
  readPrec = readPrec_

instance (Eq (v a), Vector v a) => Eq (CVector v a) where
  (CVector l1 v1) == (CVector l2 v2) = slice_ 0 l1 v1 == slice 0 l2 v2
  {-# INLINE (==) #-}

instance (Ord (v a), Vector v a) => Ord (CVector v a) where
  compare (CVector l1 v1) (CVector l2 v2) =
    compare (slice_ 0 l1 v1) (slice_ 0 l2 v2)
  {-# INLINE compare #-}

instance (Monoid (v a), Vector v a) => Monoid (CVector v a) where
  mempty = empty
  {-# INLINE mempty #-}

  mappend = (++)
  {-# INLINE mappend #-}

instance (NFData (v a), Vector v a) => NFData (CVector v a) where
    rnf (CVector _ v) = rnf v

capacity :: (Vector v a) => CVector v a -> Int
capacity (CVector _ v) = Vector.length v
{-# INLINE capacity #-}

-- | Freeze the CMVector preserving the size.  Note the
-- Data.Vector.Generic.freeze will automatically shrink it to its length.
freeze :: (PrimMonad m, Vector v a)
  => Mutable (CVector v) (PrimState m) a -> m (CVector v a)
freeze (CMVector l mv) = liftM (CVector l) (Vector.freeze mv)
{-# INLINE freeze #-}

-- | Thaw the CVector preserving the size.  Note the Data.Vector.Generic.freeze
-- will automatically shrink it to its length.
thaw :: (PrimMonad m, Vector v a)
  => CVector v a -> m (Mutable (CVector v) (PrimState m) a)
thaw (CVector l v) = liftM (CMVector l) (Vector.thaw v)
{-# INLINE thaw #-}

showsPrec_ :: (Vector v a, Show (v a)) => Int -> CVector v a -> ShowS
showsPrec_ p (CVector l v) = -- showParen (p > 10) $ showString "fromList " . shows (toList v)
  showParen (10 < p)
  $ showString "CVector "
  . shows (Vector.length v)
  . showChar ' '
  . showsPrec 11 (slice_ 0 l v)
{-# INLINE showsPrec_ #-}

readPrec_ :: forall a v. (Read a, Vector v a) => Read.ReadPrec (CVector v a)
readPrec_ = Read.parens $ Read.prec 10 $ do
  Read.Ident "C" <- Read.lexP
  s <- Read.readPrec
  xs <- Read.parens $ do
    Read.Ident "fromList" <- Read.lexP
    Read.readPrec
  let v = runST st
      st :: forall s. ST s (v a)
      st = do
        mv <- MVector.new s
        Prelude.mapM_ (uncurry $ write_ mv)
                      (Prelude.zip [0.. s] xs)
        Vector.unsafeFreeze mv
  return $ CVector (Prelude.length xs) v
{-# INLINE readPrec_ #-}

slice_ :: (Vector v a) => Int -> Int -> v a -> v a
#ifdef BOUNDS_CHECKING
slice_ = Vector.slice
#else
slice_ = Vector.unsafeSlice
#endif
{-# INLINE slice_ #-}

write_ :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> a -> m ()
#ifdef BOUNDS_CHECKING
write_ = MVector.write
#else
write_ = MVector.unsafeWrite
#endif
