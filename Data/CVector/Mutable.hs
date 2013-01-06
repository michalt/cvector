-- |
-- Module      : Data.CVector.Mutable
-- Copyright   : (c) 2012-2013 Michal Terepeta
--               (c) 2008-2010 Roman Leshchinskiy
-- License     : BSD-style
--
-- Maintainer  : Michal Terepeta <michal.terepeta@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Wrapper around Data.Vector.Mutable implementing efficient pushBack and
-- popBack operations.
--

module Data.CVector.Mutable (

  -- * CVector specific
  CMVector, IOCVector, STCVector,

  capacity, fromVector, toVector, pushBack, popBack,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Accessing individual elements
  read, write, swap,
  unsafeRead, unsafeWrite, unsafeSwap,

  -- * Modifying vectors

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove
) where

import Prelude ( Bool, Enum, Eq, Functor, Int, Maybe, Monad, Num, Ord, Ordering )
import qualified Prelude

import Data.Vector.Mutable ( MVector(..), MVector )
import Control.Monad.Primitive

import qualified Data.CVector.Generic.Mutable as G

--
-- CVector specific
--

type CMVector = G.CMVector MVector

type IOCVector = CMVector RealWorld
type STCVector s = CMVector s

-- | Yields the allocated size of the underlying 'Vector' (not the number of
-- elements kept in the CMVector).  /O(1)/
capacity :: CMVector s a -> Int
capacity = G.capacity
{-# INLINE capacity #-}

-- | Create a CMVector from MVector.  /O(1)/
fromVector :: MVector s a -> CMVector s a
fromVector = G.fromVector
{-# INLINE fromVector #-}

-- | Convert the CMVector to MVector (using 'slice').  /O(1)/
toVector :: CMVector s a -> MVector s a
toVector = G.toVector
{-# INLINE toVector #-}

-- | Push an element at the back of the CVector.  If the size of the CVector and
-- its length are equal (i.e., it's full), the underlying Vector will be doubled
-- in size.  Otherwise no allocation will be done and the underlying vector will
-- be shared between the argument and the result.  /amortized O(1)/
pushBack :: (PrimMonad m)
  => CMVector (PrimState m) a -> a -> m (CMVector (PrimState m) a)
pushBack = G.pushBack
{-# INLINE pushBack #-}

-- | Remove an element from the back of the CVector.  Calls 'error' if the
-- CVector is empty.  Does not shrink the underlying Vector.  /O(1)/
popBack :: (PrimMonad m)
  => CMVector (PrimState m) a -> m (CMVector (PrimState m) a)
popBack = G.popBackOverwrite
{-# INLINE popBack #-}

--
-- Length information
--

length :: CMVector s a -> Int
length = G.length
{-# INLINE length #-}

null :: CMVector s a -> Bool
null = G.null
{-# INLINE null #-}

--
-- Extracting subvectors
--

slice :: Int -> Int -> CMVector s a -> CMVector s a
slice = G.slice
{-# INLINE slice #-}

take :: Int -> CMVector s a -> CMVector s a
take = G.take
{-# INLINE take #-}

drop :: Int -> CMVector s a -> CMVector s a
drop = G.drop
{-# INLINE drop #-}

splitAt :: Int -> CMVector s a -> (CMVector s a, CMVector s a)
splitAt = G.splitAt
{-# INLINE splitAt #-}

init :: CMVector s a -> CMVector s a
init = G.init
{-# INLINE init #-}

tail :: CMVector s a -> CMVector s a
tail = G.tail
{-# INLINE tail #-}

unsafeSlice :: Int -> Int -> CMVector s a -> CMVector s a
unsafeSlice = G.unsafeSlice
{-# INLINE unsafeSlice #-}

unsafeTake :: Int -> CMVector s a -> CMVector s a
unsafeTake = G.unsafeTake
{-# INLINE unsafeTake #-}

unsafeDrop :: Int -> CMVector s a -> CMVector s a
unsafeDrop = G.unsafeDrop
{-# INLINE unsafeDrop #-}

unsafeInit :: CMVector s a -> CMVector s a
unsafeInit = G.unsafeInit
{-# INLINE unsafeInit #-}

unsafeTail :: CMVector s a -> CMVector s a
unsafeTail = G.unsafeTail
{-# INLINE unsafeTail #-}

--
-- Overlapping
--

overlaps :: CMVector s a -> CMVector s a -> Bool
overlaps = G.overlaps
{-# INLINE overlaps #-}

--
-- Initialisation
--

new :: (PrimMonad m) => Int -> m (CMVector (PrimState m) a)
new = G.new
{-# INLINE new #-}

unsafeNew :: (PrimMonad m) => Int -> m (CMVector (PrimState m) a)
unsafeNew = G.unsafeNew
{-# INLINE unsafeNew #-}

replicate :: (PrimMonad m) => Int -> a -> m (CMVector (PrimState m) a)
replicate = G.replicate
{-# INLINE replicate #-}

replicateM :: (PrimMonad m) => Int -> m a -> m (CMVector (PrimState m) a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

clone :: (PrimMonad m) => CMVector (PrimState m) a -> m (CMVector (PrimState m) a)
clone = G.clone
{-# INLINE clone #-}

--
-- Growing
--

grow :: (PrimMonad m)
  => CMVector (PrimState m) a -> Int -> m (CMVector (PrimState m) a)
grow = G.grow
{-# INLINE grow #-}

unsafeGrow :: (PrimMonad m)
  => CMVector (PrimState m) a -> Int -> m (CMVector (PrimState m) a)
unsafeGrow = G.unsafeGrow
{-# INLINE unsafeGrow #-}

--
-- Restricting memory usage
--

clear :: (PrimMonad m) => CMVector (PrimState m) a -> m ()
clear = G.clear
{-# INLINE clear #-}

--
-- Accessing individual elements
--

read :: (PrimMonad m) => CMVector (PrimState m) a -> Int -> m a
read = G.read
{-# INLINE read #-}

write :: (PrimMonad m) => CMVector (PrimState m) a -> Int -> a -> m ()
write = G.write
{-# INLINE write #-}

swap :: (PrimMonad m) => CMVector (PrimState m) a -> Int -> Int -> m ()
swap = G.swap
{-# INLINE swap #-}


unsafeRead :: (PrimMonad m) => CMVector (PrimState m) a -> Int -> m a
unsafeRead = G.unsafeRead
{-# INLINE unsafeRead #-}

unsafeWrite :: (PrimMonad m) => CMVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite = G.unsafeWrite
{-# INLINE unsafeWrite #-}

unsafeSwap :: (PrimMonad m) => CMVector (PrimState m) a -> Int -> Int -> m ()
unsafeSwap = G.unsafeSwap
{-# INLINE unsafeSwap #-}

--
-- Filling and copying
--

set :: (PrimMonad m) => CMVector (PrimState m) a -> a -> m ()
set = G.set
{-# INLINE set #-}

copy :: (PrimMonad m)
  => CMVector (PrimState m) a -> CMVector (PrimState m) a -> m ()
copy = G.copy
{-# INLINE copy #-}

unsafeCopy :: (PrimMonad m)
  => CMVector (PrimState m) a -> CMVector (PrimState m) a -> m ()
unsafeCopy = G.unsafeCopy
{-# INLINE unsafeCopy #-}

move :: (PrimMonad m)
  => CMVector (PrimState m) a -> CMVector (PrimState m) a -> m ()
move = G.move
{-# INLINE move #-}

unsafeMove :: (PrimMonad m)
  => CMVector (PrimState m) a -> CMVector (PrimState m) a -> m ()
unsafeMove = G.unsafeMove
{-# INLINE unsafeMove #-}
