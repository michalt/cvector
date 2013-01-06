{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.CVector.Unboxed
-- Copyright   : (c) 2012-2013 Michal Terepeta
--               (c) 2009-2010 Roman Leshchinskiy
-- License     : BSD-style
--
-- Maintainer  : Michal Terepeta <michal.terepeta@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Wrapper around unboxed 'Vector's; its mutable version implements efficient
-- pushBack and popBack operations.
--

module Data.CVector.Unboxed (

  -- * CVector specifc
  CVector, Vector, Unbox,

  capacity, fromVector, toVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic indexing
  indexM, headM, lastM,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, create,

  -- ** Unfolding
  unfoldr, unfoldrN,
  constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update, update_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate, accumulate_,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Indexing
  indexed,

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, mapM_, forM, forM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, zipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, foldM', fold1M, fold1M',
  foldM_, foldM'_, fold1M_, fold1M'_,

  -- * Prefix sums (scans)
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- * Conversions

  -- ** Lists
  toList, fromList, fromListN,

  -- ** Other vector types
  G.convert,

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy
) where

import Prelude ( Bool, Enum, Eq, Int, Maybe, Monad, Num, Ord, Ordering )
import qualified Prelude

import Control.Monad.ST ( ST )
import Control.Monad.Primitive ( PrimMonad, PrimState )

import Data.Vector.Unboxed ( Unbox, Vector )
import qualified Data.Vector.Unboxed as UVector

import Data.CVector.Unboxed.Mutable ( CMVector )
import qualified Data.CVector.Generic as G
import qualified Data.CVector.GenericInternal as GI

--
-- CVector specific
--

type CVector = GI.CVector Vector

capacity :: (Unbox a) => CVector a -> Int
capacity = G.capacity
{-# INLINE capacity #-}

fromVector :: (Unbox a) => Vector a -> CVector a
fromVector = G.fromVector
{-# INLINE fromVector #-}

toVector :: CVector a -> Vector a
toVector = G.toVector
{-# INLINE toVector #-}

--
-- Length information
--

length :: (Unbox a) => CVector a -> Int
length = G.length
{-# INLINE length #-}

null :: (Unbox a) => CVector a -> Bool
null = G.null
{-# INLINE null #-}

--
-- Indexing
--

(!) :: (Unbox a) => CVector a -> Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

(!?) :: (Unbox a) => CVector a -> Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

head :: (Unbox a) => CVector a -> a
head = G.head
{-# INLINE head #-}

last :: (Unbox a) => CVector a -> a
last = G.last
{-# INLINE last #-}

unsafeIndex :: (Unbox a) => CVector a -> Int -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

unsafeHead :: (Unbox a) => CVector a -> a
unsafeHead = G.unsafeHead
{-# INLINE unsafeHead #-}

unsafeLast :: (Unbox a) => CVector a -> a
unsafeLast = G.unsafeLast
{-# INLINE unsafeLast #-}

--
-- Monadic indexing
--

indexM :: (Unbox a, Monad m) => CVector a -> Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

headM :: (Unbox a, Monad m) => CVector a -> m a
headM = G.headM
{-# INLINE headM #-}

lastM :: (Unbox a, Monad m) => CVector a -> m a
lastM = G.lastM
{-# INLINE lastM #-}

unsafeIndexM :: (Unbox a, Monad m) => CVector a -> Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

unsafeHeadM :: (Unbox a, Monad m) => CVector a -> m a
unsafeHeadM = G.unsafeHeadM
{-# INLINE unsafeHeadM #-}

unsafeLastM :: (Unbox a, Monad m) => CVector a -> m a
unsafeLastM = G.unsafeLastM
{-# INLINE unsafeLastM #-}

--
-- Extracting subvectors (slicing)
--

slice :: (Unbox a) => Int
                 -> Int
                 -> CVector a
                 -> CVector a
slice = G.slice
{-# INLINE slice #-}

init :: (Unbox a) => CVector a -> CVector a
init = G.init
{-# INLINE init #-}

tail :: (Unbox a) => CVector a -> CVector a
tail = G.tail
{-# INLINE tail #-}

take :: (Unbox a) => Int -> CVector a -> CVector a
take = G.take
{-# INLINE take #-}

drop :: (Unbox a) => Int -> CVector a -> CVector a
drop = G.drop
{-# INLINE drop #-}

splitAt :: (Unbox a) => Int -> CVector a -> (CVector a, CVector a)
splitAt = G.splitAt
{-# INLINE splitAt #-}

unsafeSlice :: (Unbox a) => Int -> Int -> CVector a -> CVector a
unsafeSlice = G.unsafeSlice
{-# INLINE unsafeSlice #-}

unsafeInit :: (Unbox a) => CVector a -> CVector a
unsafeInit = G.unsafeInit
{-# INLINE unsafeInit #-}

unsafeTail :: (Unbox a) => CVector a -> CVector a
unsafeTail = G.unsafeTail
{-# INLINE unsafeTail #-}

unsafeTake :: (Unbox a) => Int -> CVector a -> CVector a
unsafeTake = G.unsafeTake
{-# INLINE unsafeTake #-}

unsafeDrop :: (Unbox a) => Int -> CVector a -> CVector a
unsafeDrop = G.unsafeDrop
{-# INLINE unsafeDrop #-}

--
-- Initialisation
--

empty :: (Unbox a) => CVector a
empty = G.empty
{-# INLINE empty #-}

singleton :: (Unbox a) => a -> CVector a
singleton = G.singleton
{-# INLINE singleton #-}

replicate :: (Unbox a) => Int -> a -> CVector a
replicate = G.replicate
{-# INLINE replicate #-}

generate :: (Unbox a) => Int -> (Int -> a) -> CVector a
generate = G.generate
{-# INLINE generate #-}

iterateN :: (Unbox a) => Int -> (a -> a) -> a -> CVector a
iterateN = G.iterateN
{-# INLINE iterateN #-}

--
-- Unfolding
--

unfoldr :: (Unbox a) => (b -> Maybe (a, b)) -> b -> CVector a
unfoldr = G.unfoldr
{-# INLINE unfoldr #-}

unfoldrN :: (Unbox a) => Int -> (b -> Maybe (a, b)) -> b -> CVector a
unfoldrN = G.unfoldrN
{-# INLINE unfoldrN #-}

constructN :: (Unbox a) => Int -> (CVector a -> a) -> CVector a
constructN = G.constructN
{-# INLINE constructN #-}

constructrN :: (Unbox a) => Int -> (CVector a -> a) -> CVector a
constructrN = G.constructrN
{-# INLINE constructrN #-}

--
-- Enumeration
--

enumFromN :: (Unbox a, Num a) => a -> Int -> CVector a
enumFromN = G.enumFromN
{-# INLINE enumFromN #-}

enumFromStepN :: (Unbox a, Num a) => a -> a -> Int -> CVector a
enumFromStepN = G.enumFromStepN
{-# INLINE enumFromStepN #-}

enumFromTo :: (Unbox a, Enum a) => a -> a -> CVector a
enumFromTo = G.enumFromTo
{-# INLINE enumFromTo #-}

enumFromThenTo :: (Unbox a, Enum a) => a -> a -> a -> CVector a
enumFromThenTo = G.enumFromThenTo
{-# INLINE enumFromThenTo #-}

-- Concatenation
-- -------------

cons :: (Unbox a) => a -> CVector a -> CVector a
cons = G.cons
{-# INLINE cons #-}

snoc :: (Unbox a) => CVector a -> a -> CVector a
snoc = G.snoc
{-# INLINE snoc #-}

infixr 5 ++
(++) :: (Unbox a) => CVector a -> CVector a -> CVector a
(++) = (G.++)
{-# INLINE (++) #-}

concat :: (Unbox a) => [CVector a] -> CVector a
concat = G.concat
{-# INLINE concat #-}

--
-- Monadic initialisation
--

replicateM :: (Monad m, Unbox a) => Int -> m a -> m (CVector a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

generateM :: (Monad m, Unbox a) => Int -> (Int -> m a) -> m (CVector a)
generateM = G.generateM
{-# INLINE generateM #-}

create :: (Unbox a) => (forall s. ST s (CMVector s a)) -> CVector a
create = G.create
{-# INLINE create #-}

--
-- Restricting memory usage
--

force :: (Unbox a) => CVector a -> CVector a
force = G.force
{-# INLINE force #-}

--
-- Bulk updates
--

(//) :: (Unbox a) => CVector a -> [(Int, a)] -> CVector a
(//) = (G.//)
{-# INLINE (//) #-}

update :: Unbox a => CVector a -> CVector (Int, a) -> CVector a
update = G.update
{-# INLINE update #-}

update_ :: Unbox a => CVector a -> CVector Int -> CVector a -> CVector a
update_ = G.update_
{-# INLINE update_ #-}

unsafeUpd :: (Unbox a) => CVector a -> [(Int, a)] -> CVector a
unsafeUpd = G.unsafeUpd
{-# INLINE unsafeUpd #-}

unsafeUpdate :: (Unbox a) => CVector a -> CVector (Int, a) -> CVector a
unsafeUpdate = G.unsafeUpdate
{-# INLINE unsafeUpdate #-}

unsafeUpdate_ :: (Unbox a) => CVector a -> CVector Int -> CVector a -> CVector a
unsafeUpdate_ = G.unsafeUpdate_
{-# INLINE unsafeUpdate_ #-}

--
-- Accumulations
--

accum :: Unbox a => (a -> b -> a) -> CVector a -> [(Int,b)] -> CVector a
accum = G.accum
{-# INLINE accum #-}

accumulate :: (Unbox a, Unbox b)
  => (a -> b -> a) -> CVector a -> CVector (Int,b) -> CVector a
accumulate = G.accumulate
{-# INLINE accumulate #-}

accumulate_ :: (Unbox a, Unbox b)
  => (a -> b -> a) -> CVector a -> CVector Int -> CVector b -> CVector a
accumulate_ = G.accumulate_
{-# INLINE accumulate_ #-}

unsafeAccum :: (Unbox a) => (a -> b -> a) -> CVector a -> [(Int,b)] -> CVector a
unsafeAccum = G.unsafeAccum
{-# INLINE unsafeAccum #-}

unsafeAccumulate :: (Unbox a, Unbox b)
                => (a -> b -> a) -> CVector a -> CVector (Int,b) -> CVector a
unsafeAccumulate = G.unsafeAccumulate
{-# INLINE unsafeAccumulate #-}

unsafeAccumulate_ :: (Unbox a, Unbox b) =>
               (a -> b -> a) -> CVector a -> CVector Int -> CVector b -> CVector a
unsafeAccumulate_ = G.unsafeAccumulate_
{-# INLINE unsafeAccumulate_ #-}

--
-- Permutations
--

reverse :: (Unbox a) => CVector a -> CVector a
reverse = G.reverse
{-# INLINE reverse #-}

backpermute :: (Unbox a) => CVector a -> CVector Int -> CVector a
backpermute = G.backpermute
{-# INLINE backpermute #-}

unsafeBackpermute :: (Unbox a) => CVector a -> CVector Int -> CVector a
unsafeBackpermute = G.unsafeBackpermute
{-# INLINE unsafeBackpermute #-}

--
-- Safe destructive updates
--

modify :: (Unbox a)
  => (forall s. CMVector s a -> ST s ()) -> CVector a -> CVector a
modify p = G.modify p
{-# INLINE modify #-}

--
-- Indexing
--

indexed :: (Unbox a) => CVector a -> CVector (Int,a)
indexed = G.indexed
{-# INLINE indexed #-}

--
-- Mapping
--

map :: (Unbox a, Unbox b) => (a -> b) -> CVector a -> CVector b
map = G.map
{-# INLINE map #-}

imap :: (Unbox a, Unbox b) => (Int -> a -> b) -> CVector a -> CVector b
imap = G.imap
{-# INLINE imap #-}

concatMap :: (Unbox a, Unbox b) => (a -> CVector b) -> CVector a -> CVector b
concatMap = G.concatMap
{-# INLINE concatMap #-}

--
-- Monadic mapping
--

mapM :: (Monad m, Unbox a, Unbox b) => (a -> m b) -> CVector a -> m (CVector b)
mapM = G.mapM
{-# INLINE mapM #-}

mapM_ :: (Monad m, Unbox a) => (a -> m b) -> CVector a -> m ()
mapM_ = G.mapM_
{-# INLINE mapM_ #-}

forM :: (Monad m, Unbox a, Unbox b) => CVector a -> (a -> m b) -> m (CVector b)
forM = G.forM
{-# INLINE forM #-}

forM_ :: (Monad m, Unbox a) => CVector a -> (a -> m b) -> m ()
forM_ = G.forM_
{-# INLINE forM_ #-}

--
-- Zipping
--

zipWith :: (Unbox a, Unbox b, Unbox c)
  => (a -> b -> c) -> CVector a -> CVector b -> CVector c
zipWith = G.zipWith
{-# INLINE zipWith #-}

zipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
  => (a -> b -> c -> d) -> CVector a -> CVector b -> CVector c -> CVector d
zipWith3 = G.zipWith3
{-# INLINE zipWith3 #-}

zipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
  => (a -> b -> c -> d -> e)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
zipWith4 = G.zipWith4
{-# INLINE zipWith4 #-}

zipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
  => (a -> b -> c -> d -> e -> f)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e -> CVector f
zipWith5 = G.zipWith5
{-# INLINE zipWith5 #-}

zipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
  => (a -> b -> c -> d -> e -> f -> g)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector f -> CVector g
zipWith6 = G.zipWith6
{-# INLINE zipWith6 #-}

izipWith :: (Unbox a, Unbox b, Unbox c)
  => (Int -> a -> b -> c) -> CVector a -> CVector b -> CVector c
izipWith = G.izipWith
{-# INLINE izipWith #-}

izipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d)
  => (Int -> a -> b -> c -> d) -> CVector a -> CVector b -> CVector c -> CVector d
izipWith3 = G.izipWith3
{-# INLINE izipWith3 #-}

izipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
  => (Int -> a -> b -> c -> d -> e)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
izipWith4 = G.izipWith4
{-# INLINE izipWith4 #-}

izipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
  => (Int -> a -> b -> c -> d -> e -> f)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e -> CVector f
izipWith5 = G.izipWith5
{-# INLINE izipWith5 #-}

izipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g)
  => (Int -> a -> b -> c -> d -> e -> f -> g)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector f -> CVector g
izipWith6 = G.izipWith6
{-# INLINE izipWith6 #-}

zip :: (Unbox a, Unbox b)
  => CVector a -> CVector b -> CVector (a, b)
zip (GI.CVector l1 v1) (GI.CVector l2 v2) =
  GI.CVector (Prelude.min l1 l2) (UVector.zip v1 v2)
{-# INLINE zip #-}

zip3 :: (Unbox a, Unbox b, Unbox c)
  => CVector a -> CVector b -> CVector c -> CVector (a, b, c)
zip3 (GI.CVector l1 v1) (GI.CVector l2 v2) (GI.CVector l3 v3) =
  GI.CVector (Prelude.minimum [l1, l2, l3]) (UVector.zip3 v1 v2 v3)
{-# INLINE zip3 #-}

zip4 :: (Unbox a, Unbox b, Unbox c, Unbox d)
  => CVector a -> CVector b -> CVector c -> CVector d -> CVector (a, b, c, d)
zip4 (GI.CVector l1 v1) (GI.CVector l2 v2) (GI.CVector l3 v3) (GI.CVector l4 v4) =
  GI.CVector (Prelude.minimum [l1, l2, l3, l4]) (UVector.zip4 v1 v2 v3 v4)
{-# INLINE zip4 #-}

zip5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
  => CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector (a, b, c, d, e)
zip5 (GI.CVector l1 v1) (GI.CVector l2 v2) (GI.CVector l3 v3) (GI.CVector l4 v4)
  (GI.CVector l5 v5) = GI.CVector (Prelude.minimum [l1, l2, l3, l4, l5])
                                (UVector.zip5 v1 v2 v3 v4 v5)
{-# INLINE zip5 #-}

zip6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
  => CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector f -> CVector (a, b, c, d, e, f)
zip6 (GI.CVector l1 v1) (GI.CVector l2 v2) (GI.CVector l3 v3)
     (GI.CVector l4 v4) (GI.CVector l5 v5) (GI.CVector l6 v6) =
  GI.CVector (Prelude.minimum [l1, l2, l3, l4, l5, l6])
            (UVector.zip6 v1 v2 v3 v4 v5 v6)
{-# INLINE zip6 #-}

--
-- Monadic zipping
--

zipWithM :: (Monad m, Unbox a, Unbox b, Unbox c)
  => (a -> b -> m c) -> CVector a -> CVector b -> m (CVector c)
zipWithM = G.zipWithM
{-# INLINE zipWithM #-}

zipWithM_ :: (Monad m, Unbox a, Unbox b)
  => (a -> b -> m c) -> CVector a -> CVector b -> m ()
zipWithM_ = G.zipWithM_
{-# INLINE zipWithM_ #-}

--
-- Unzipping
--

unzip :: (Unbox a, Unbox b) => CVector (a, b) -> (CVector a, CVector b)
unzip (GI.CVector l v) = (GI.CVector l v1, GI.CVector l v2)
  where
    (v1, v2) = UVector.unzip v

unzip3 :: (Unbox a, Unbox b, Unbox c)
  => CVector (a, b, c)
  -> (CVector a, CVector b, CVector c)
unzip3 (GI.CVector l v) = (GI.CVector l v1, GI.CVector l v2, GI.CVector l v3)
  where
    (v1, v2, v3) = UVector.unzip3 v

unzip4 :: (Unbox a, Unbox b, Unbox c, Unbox d)
  => CVector (a, b, c, d)
  -> (CVector a, CVector b, CVector c, CVector d)
unzip4 (GI.CVector l v) =
  (GI.CVector l v1, GI.CVector l v2, GI.CVector l v3, GI.CVector l v4)
  where
    (v1, v2, v3, v4) = UVector.unzip4 v

unzip5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e)
  => CVector (a, b, c, d, e)
  -> (CVector a, CVector b, CVector c, CVector d, CVector e)
unzip5 (GI.CVector l v) =
  let (v1, v2, v3, v4, v5) = UVector.unzip5 v
  in (GI.CVector l v1, GI.CVector l v2, GI.CVector l v3,
      GI.CVector l v4, GI.CVector l v5)

unzip6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f)
  => CVector (a, b, c, d, e, f)
  -> (CVector a, CVector b, CVector c, CVector d, CVector e, CVector f)
unzip6 (GI.CVector l v) = (GI.CVector l v1, GI.CVector l v2, GI.CVector l v3,
                          GI.CVector l v4, GI.CVector l v5, GI.CVector l v6)
  where
    (v1, v2, v3, v4, v5, v6) = UVector.unzip6 v

--
-- Filtering
--

filter :: (Unbox a) => (a -> Bool) -> CVector a -> CVector a
filter = G.filter
{-# INLINE filter #-}

ifilter :: (Unbox a) => (Int -> a -> Bool) -> CVector a -> CVector a
ifilter = G.ifilter
{-# INLINE ifilter #-}

filterM :: (Monad m, Unbox a) => (a -> m Bool) -> CVector a -> m (CVector a)
filterM = G.filterM
{-# INLINE filterM #-}

takeWhile :: (Unbox a) => (a -> Bool) -> CVector a -> CVector a
takeWhile = G.takeWhile
{-# INLINE takeWhile #-}

dropWhile :: (Unbox a) => (a -> Bool) -> CVector a -> CVector a
dropWhile = G.dropWhile
{-# INLINE dropWhile #-}

--
-- Parititioning
--

partition :: (Unbox a) => (a -> Bool) -> CVector a -> (CVector a, CVector a)
partition = G.partition
{-# INLINE partition #-}

unstablePartition :: (Unbox a)
  => (a -> Bool) -> CVector a -> (CVector a, CVector a)
unstablePartition = G.unstablePartition
{-# INLINE unstablePartition #-}

span :: (Unbox a) => (a -> Bool) -> CVector a -> (CVector a, CVector a)
span = G.span
{-# INLINE span #-}

break :: (Unbox a) => (a -> Bool) -> CVector a -> (CVector a, CVector a)
break = G.break
{-# INLINE break #-}

--
-- Searching
--

infix 4 `elem`
elem :: (Unbox a, Eq a) => a -> CVector a -> Bool
elem = G.elem
{-# INLINE elem #-}

infix 4 `notElem`
notElem :: (Unbox a, Eq a) => a -> CVector a -> Bool
notElem = G.notElem
{-# INLINE notElem #-}

find :: (Unbox a) => (a -> Bool) -> CVector a -> Maybe a
find = G.find
{-# INLINE find #-}

findIndex :: (Unbox a) => (a -> Bool) -> CVector a -> Maybe Int
findIndex = G.findIndex
{-# INLINE findIndex #-}

findIndices :: (Unbox a) => (a -> Bool) -> CVector a -> CVector Int
findIndices = G.findIndices
{-# INLINE findIndices #-}

elemIndex :: (Unbox a, Eq a) => a -> CVector a -> Maybe Int
elemIndex = G.elemIndex
{-# INLINE elemIndex #-}

elemIndices :: (Unbox a, Eq a) => a -> CVector a -> CVector Int
elemIndices = G.elemIndices
{-# INLINE elemIndices #-}

--
-- Folding
--

foldl :: Unbox b => (a -> b -> a) -> a -> CVector b -> a
foldl = G.foldl
{-# INLINE foldl #-}

foldl1 :: (Unbox a) => (a -> a -> a) -> CVector a -> a
foldl1 = G.foldl1
{-# INLINE foldl1 #-}

foldl' :: Unbox b => (a -> b -> a) -> a -> CVector b -> a
foldl' = G.foldl'
{-# INLINE foldl' #-}

foldl1' :: (Unbox a) => (a -> a -> a) -> CVector a -> a
foldl1' = G.foldl1'
{-# INLINE foldl1' #-}

foldr :: (Unbox a) => (a -> b -> b) -> b -> CVector a -> b
foldr = G.foldr
{-# INLINE foldr #-}

foldr1 :: (Unbox a) => (a -> a -> a) -> CVector a -> a
foldr1 = G.foldr1
{-# INLINE foldr1 #-}

foldr' :: (Unbox a) => (a -> b -> b) -> b -> CVector a -> b
foldr' = G.foldr'
{-# INLINE foldr' #-}

foldr1' :: (Unbox a) => (a -> a -> a) -> CVector a -> a
foldr1' = G.foldr1'
{-# INLINE foldr1' #-}

ifoldl :: Unbox b => (a -> Int -> b -> a) -> a -> CVector b -> a
ifoldl = G.ifoldl
{-# INLINE ifoldl #-}

ifoldl' :: Unbox b => (a -> Int -> b -> a) -> a -> CVector b -> a
ifoldl' = G.ifoldl'
{-# INLINE ifoldl' #-}

ifoldr :: (Unbox a) => (Int -> a -> b -> b) -> b -> CVector a -> b
ifoldr = G.ifoldr
{-# INLINE ifoldr #-}

ifoldr' :: (Unbox a) => (Int -> a -> b -> b) -> b -> CVector a -> b
ifoldr' = G.ifoldr'
{-# INLINE ifoldr' #-}

--
-- Specialised folds
--

all :: (Unbox a) => (a -> Bool) -> CVector a -> Bool
all = G.all
{-# INLINE all #-}

any :: (Unbox a) => (a -> Bool) -> CVector a -> Bool
any = G.any
{-# INLINE any #-}

and :: CVector Bool -> Bool
and = G.and
{-# INLINE and #-}

or :: CVector Bool -> Bool
or = G.or
{-# INLINE or #-}

sum :: (Unbox a, Num a) => CVector a -> a
sum = G.sum
{-# INLINE sum #-}

product :: (Unbox a, Num a) => CVector a -> a
product = G.product
{-# INLINE product #-}

maximum :: (Unbox a, Ord a) => CVector a -> a
maximum = G.maximum
{-# INLINE maximum #-}

maximumBy :: (Unbox a) => (a -> a -> Ordering) -> CVector a -> a
maximumBy = G.maximumBy
{-# INLINE maximumBy #-}

minimum :: (Unbox a, Ord a) => CVector a -> a
minimum = G.minimum
{-# INLINE minimum #-}

minimumBy :: (Unbox a) => (a -> a -> Ordering) -> CVector a -> a
minimumBy = G.minimumBy
{-# INLINE minimumBy #-}

maxIndex :: (Unbox a, Ord a) => CVector a -> Int
maxIndex = G.maxIndex
{-# INLINE maxIndex #-}

maxIndexBy :: (Unbox a) => (a -> a -> Ordering) -> CVector a -> Int
maxIndexBy = G.maxIndexBy
{-# INLINE maxIndexBy #-}

minIndex :: (Unbox a, Ord a) => CVector a -> Int
minIndex = G.minIndex
{-# INLINE minIndex #-}

minIndexBy :: (Unbox a) => (a -> a -> Ordering) -> CVector a -> Int
minIndexBy = G.minIndexBy
{-# INLINE minIndexBy #-}

--
-- Monadic folds
--

foldM :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> CVector b -> m a
foldM = G.foldM
{-# INLINE foldM #-}

fold1M :: (Monad m, Unbox a) => (a -> a -> m a) -> CVector a -> m a
fold1M = G.fold1M
{-# INLINE fold1M #-}

foldM' :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> CVector b -> m a
foldM' = G.foldM'
{-# INLINE foldM' #-}

fold1M' :: (Monad m, Unbox a) => (a -> a -> m a) -> CVector a -> m a
fold1M' = G.fold1M'
{-# INLINE fold1M' #-}

foldM_ :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> CVector b -> m ()
foldM_ = G.foldM_
{-# INLINE foldM_ #-}

fold1M_ :: (Monad m, Unbox a) => (a -> a -> m a) -> CVector a -> m ()
fold1M_ = G.fold1M_
{-# INLINE fold1M_ #-}

foldM'_ :: (Monad m, Unbox b) => (a -> b -> m a) -> a -> CVector b -> m ()
foldM'_ = G.foldM'_
{-# INLINE foldM'_ #-}

fold1M'_ :: (Monad m, Unbox a) => (a -> a -> m a) -> CVector a -> m ()
fold1M'_ = G.fold1M'_
{-# INLINE fold1M'_ #-}

--
-- Prefix sums (scans)
--

prescanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> CVector b -> CVector a
prescanl = G.prescanl
{-# INLINE prescanl #-}

prescanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> CVector b -> CVector a
prescanl' = G.prescanl'
{-# INLINE prescanl' #-}

postscanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> CVector b -> CVector a
postscanl = G.postscanl
{-# INLINE postscanl #-}

postscanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> CVector b -> CVector a
postscanl' = G.postscanl'
{-# INLINE postscanl' #-}

scanl :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> CVector b -> CVector a
scanl = G.scanl
{-# INLINE scanl #-}

scanl' :: (Unbox a, Unbox b) => (a -> b -> a) -> a -> CVector b -> CVector a
scanl' = G.scanl'
{-# INLINE scanl' #-}

scanl1 :: (Unbox a) => (a -> a -> a) -> CVector a -> CVector a
scanl1 = G.scanl1
{-# INLINE scanl1 #-}

scanl1' :: (Unbox a) => (a -> a -> a) -> CVector a -> CVector a
scanl1' = G.scanl1'
{-# INLINE scanl1' #-}

prescanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> CVector a -> CVector b
prescanr = G.prescanr
{-# INLINE prescanr #-}

prescanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> CVector a -> CVector b
prescanr' = G.prescanr'
{-# INLINE prescanr' #-}

postscanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> CVector a -> CVector b
postscanr = G.postscanr
{-# INLINE postscanr #-}

postscanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> CVector a -> CVector b
postscanr' = G.postscanr'
{-# INLINE postscanr' #-}

scanr :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> CVector a -> CVector b
scanr = G.scanr
{-# INLINE scanr #-}

scanr' :: (Unbox a, Unbox b) => (a -> b -> b) -> b -> CVector a -> CVector b
scanr' = G.scanr'
{-# INLINE scanr' #-}

scanr1 :: (Unbox a) => (a -> a -> a) -> CVector a -> CVector a
scanr1 = G.scanr1
{-# INLINE scanr1 #-}

scanr1' :: (Unbox a) => (a -> a -> a) -> CVector a -> CVector a
scanr1' = G.scanr1'
{-# INLINE scanr1' #-}

--
-- Conversions - Lists
--

toList :: (Unbox a) => CVector a -> [a]
toList = G.toList
{-# INLINE toList #-}

fromList :: (Unbox a) => [a] -> CVector a
fromList = G.fromList
{-# INLINE fromList #-}

fromListN :: (Unbox a) => Int -> [a] -> CVector a
fromListN = G.fromListN
{-# INLINE fromListN #-}

--
-- Conversions - Mutable vectors
--

unsafeFreeze :: (Unbox a, PrimMonad m) => CMVector (PrimState m) a -> m (CVector a)
unsafeFreeze = G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

unsafeThaw :: (Unbox a, PrimMonad m) => CVector a -> m (CMVector (PrimState m) a)
unsafeThaw = G.unsafeThaw
{-# INLINE unsafeThaw #-}

thaw :: (Unbox a, PrimMonad m) => CVector a -> m (CMVector (PrimState m) a)
thaw = G.thaw
{-# INLINE thaw #-}

freeze :: (Unbox a, PrimMonad m) => CMVector (PrimState m) a -> m (CVector a)
freeze = G.freeze
{-# INLINE freeze #-}

unsafeCopy
  :: (Unbox a, PrimMonad m) => CMVector (PrimState m) a -> CVector a -> m ()
unsafeCopy = G.unsafeCopy
{-# INLINE unsafeCopy #-}

copy :: (Unbox a, PrimMonad m) => CMVector (PrimState m) a -> CVector a -> m ()
copy = G.copy
{-# INLINE copy #-}
