{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.CVector
-- Copyright   : (c) 2012-2013 Michal Terepeta
--               (c) 2008-2010 Roman Leshchinskiy
-- License     : BSD-style
--
-- Maintainer  : Michal Terepeta <michal.terepeta@gmail.com>
-- Stability   : experimental
-- Portability : non-portable
--
-- Wrapper around Data.Vector.Vector; its mutable version implements efficient
-- pushBack and popBack operations.
--

module Data.CVector (

  -- * Cvector specific

  CVector,

  capacity, fromVector, toVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic indexing
  indexM, headM, lastM, unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt, unsafeSlice, unsafeInit, unsafeTail,
  unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, create,

  -- ** Unfolding
  unfoldr, unfoldrN, constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update, update_, unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate, accumulate_, unsafeAccum, unsafeAccumulate,
  unsafeAccumulate_,

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
  filter, ifilter, filterM, takeWhile, dropWhile,

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

  -- ** Monadic sequencing
  sequence, sequence_,

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

import Prelude ( Bool, Enum, Eq, Functor, Int, Maybe, Monad, Num, Ord, Ordering )
import qualified Prelude

import Control.Monad ( MonadPlus(..), ap )
import Control.Monad.ST ( ST )
import Control.Monad.Primitive ( PrimMonad, PrimState )

import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import Data.Vector ( Vector )

import Data.CVector.Mutable ( CMVector )
import qualified Data.CVector.Generic as G


--
-- CVector specific
--

type CVector = G.CVector Vector

-- | Yields the allocated size of the underlying 'Vector' (not the number of
-- elements kept in the CMVector).  /O(1)/
capacity :: CVector a -> Int
capacity = G.capacity
{-# INLINE capacity #-}

-- | Create a CMVector from MVector.  /O(1)/
fromVector :: Vector a -> CVector a
fromVector = G.fromVector
{-# INLINE fromVector #-}

-- | Convert the CMVector to MVector (using 'slice').  /O(1)/
toVector :: CVector a -> Vector a
toVector = G.toVector
{-# INLINE toVector #-}

--
-- Instances
--

instance Functor CVector where
  fmap = map
  {-# INLINE fmap #-}

instance Monad CVector where
  return = singleton
  {-# INLINE return #-}

  (>>=) = Prelude.flip concatMap
  {-# INLINE (>>=) #-}

instance MonadPlus CVector where
  mzero = empty
  {-# INLINE mzero #-}

  mplus = (++)
  {-# INLINE mplus #-}

instance Applicative.Applicative CVector where
  pure = singleton
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Applicative.Alternative CVector where
  empty = empty
  {-# INLINE empty #-}

  (<|>) = (++)
  {-# INLINE (<|>) #-}

instance Foldable.Foldable CVector where
  foldr = foldr
  {-# INLINE foldr #-}

  foldl = foldl
  {-# INLINE foldl #-}

  foldr1 = foldr1
  {-# INLINE foldr1 #-}

  foldl1 = foldl1
  {-# INLINE foldl1 #-}

instance Traversable.Traversable CVector where
  traverse f xs = fromList Applicative.<$> Traversable.traverse f (toList xs)
  {-# INLINE traverse #-}

  mapM = mapM
  {-# INLINE mapM #-}

  sequence = sequence
  {-# INLINE sequence #-}

--
-- Length information
--

length :: CVector a -> Int
length = G.length
{-# INLINE length #-}

null :: CVector a -> Bool
null = G.null
{-# INLINE null #-}

--
-- Indexing
--

(!) :: CVector a -> Int -> a
(!) = (G.!)
{-# INLINE (!) #-}

(!?) :: CVector a -> Int -> Maybe a
(!?) = (G.!?)
{-# INLINE (!?) #-}

head :: CVector a -> a
head = G.head
{-# INLINE head #-}

last :: CVector a -> a
last = G.last
{-# INLINE last #-}

unsafeIndex :: CVector a -> Int -> a
unsafeIndex = G.unsafeIndex
{-# INLINE unsafeIndex #-}

unsafeHead :: CVector a -> a
unsafeHead = G.unsafeHead
{-# INLINE unsafeHead #-}

unsafeLast :: CVector a -> a
unsafeLast = G.unsafeLast
{-# INLINE unsafeLast #-}

--
-- Monadic indexing
--

indexM :: Monad m => CVector a -> Int -> m a
indexM = G.indexM
{-# INLINE indexM #-}

headM :: Monad m => CVector a -> m a
headM = G.headM
{-# INLINE headM #-}

lastM :: Monad m => CVector a -> m a
lastM = G.lastM
{-# INLINE lastM #-}

unsafeIndexM :: Monad m => CVector a -> Int -> m a
unsafeIndexM = G.unsafeIndexM
{-# INLINE unsafeIndexM #-}

unsafeHeadM :: Monad m => CVector a -> m a
unsafeHeadM = G.unsafeHeadM
{-# INLINE unsafeHeadM #-}

unsafeLastM :: Monad m => CVector a -> m a
unsafeLastM = G.unsafeLastM
{-# INLINE unsafeLastM #-}

--
-- Extracting subvectors (slicing)
--

slice :: Int -> Int -> CVector a -> CVector a
slice = G.slice
{-# INLINE slice #-}

init :: CVector a -> CVector a
init = G.init
{-# INLINE init #-}

tail :: CVector a -> CVector a
tail = G.tail
{-# INLINE tail #-}

take :: Int -> CVector a -> CVector a
take = G.take
{-# INLINE take #-}

drop :: Int -> CVector a -> CVector a
drop = G.drop
{-# INLINE drop #-}

splitAt :: Int -> CVector a -> (CVector a, CVector a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

unsafeSlice :: Int -> Int -> CVector a -> CVector a
unsafeSlice = G.unsafeSlice
{-# INLINE unsafeSlice #-}

unsafeInit :: CVector a -> CVector a
unsafeInit = G.unsafeInit
{-# INLINE unsafeInit #-}

unsafeTail :: CVector a -> CVector a
unsafeTail = G.unsafeTail
{-# INLINE unsafeTail #-}

unsafeTake :: Int -> CVector a -> CVector a
unsafeTake = G.unsafeTake
{-# INLINE unsafeTake #-}

unsafeDrop :: Int -> CVector a -> CVector a
unsafeDrop = G.unsafeDrop
{-# INLINE unsafeDrop #-}

--
-- Initialisation
--

empty :: CVector a
empty = G.empty
{-# INLINE empty #-}

singleton :: a -> CVector a
singleton = G.singleton
{-# INLINE singleton #-}

replicate :: Int -> a -> CVector a
replicate = G.replicate
{-# INLINE replicate #-}

generate :: Int -> (Int -> a) -> CVector a
generate = G.generate
{-# INLINE generate #-}

iterateN :: Int -> (a -> a) -> a -> CVector a
iterateN = G.iterateN
{-# INLINE iterateN #-}

--
-- Unfolding
--

unfoldr :: (b -> Maybe (a, b)) -> b -> CVector a
unfoldr = G.unfoldr
{-# INLINE unfoldr #-}

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> CVector a
unfoldrN = G.unfoldrN
{-# INLINE unfoldrN #-}

constructN :: Int -> (CVector a -> a) -> CVector a
constructN = G.constructN
{-# INLINE constructN #-}

constructrN :: Int -> (CVector a -> a) -> CVector a
constructrN = G.constructrN
{-# INLINE constructrN #-}

--
-- Enumeration
--

enumFromN :: Num a => a -> Int -> CVector a
enumFromN = G.enumFromN
{-# INLINE enumFromN #-}

enumFromStepN :: Num a => a -> a -> Int -> CVector a
enumFromStepN = G.enumFromStepN
{-# INLINE enumFromStepN #-}

enumFromTo :: Enum a => a -> a -> CVector a
enumFromTo = G.enumFromTo
{-# INLINE enumFromTo #-}

enumFromThenTo :: Enum a => a -> a -> a -> CVector a
enumFromThenTo = G.enumFromThenTo
{-# INLINE enumFromThenTo #-}

--
-- Concatenation
--

cons :: a -> CVector a -> CVector a
cons = G.cons
{-# INLINE cons #-}

snoc :: CVector a -> a -> CVector a
snoc = G.snoc
{-# INLINE snoc #-}

infixr 5 ++
(++) :: CVector a -> CVector a -> CVector a
(++) = (G.++)
{-# INLINE (++) #-}

concat :: [CVector a] -> CVector a
concat = G.concat
{-# INLINE concat #-}

--
-- Monadic initialisation
--

replicateM :: Monad m => Int -> m a -> m (CVector a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

generateM :: Monad m => Int -> (Int -> m a) -> m (CVector a)
generateM = G.generateM
{-# INLINE generateM #-}

create :: (forall s. ST s (CMVector s a)) -> CVector a
{-# INLINE create #-}
create p = G.create p

--
-- Restricting memory usage
--

force :: CVector a -> CVector a
force = G.force
{-# INLINE force #-}

--
-- Bulk updates
--

(//) :: CVector a -> [(Int, a)] -> CVector a
(//) = (G.//)
{-# INLINE (//) #-}

update :: CVector a -> CVector (Int, a) -> CVector a
update = G.update
{-# INLINE update #-}

update_ :: CVector a -> CVector Int -> CVector a -> CVector a
update_ = G.update_
{-# INLINE update_ #-}

unsafeUpd :: CVector a -> [(Int, a)] -> CVector a
unsafeUpd = G.unsafeUpd
{-# INLINE unsafeUpd #-}

unsafeUpdate :: CVector a -> CVector (Int, a) -> CVector a
unsafeUpdate = G.unsafeUpdate
{-# INLINE unsafeUpdate #-}

unsafeUpdate_ :: CVector a -> CVector Int -> CVector a -> CVector a
unsafeUpdate_ = G.unsafeUpdate_
{-# INLINE unsafeUpdate_ #-}

--
-- Accumulations
--

accum :: (a -> b -> a) -> CVector a -> [(Int,b)] -> CVector a
accum = G.accum
{-# INLINE accum #-}

accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
            -> CVector a       -- ^ initial vector (of length @m@)
            -> CVector (Int,b) -- ^ vector of index/value pairs (of length @n@)
            -> CVector a
accumulate = G.accumulate
{-# INLINE accumulate #-}

accumulate_ :: (a -> b -> a) -- ^ accumulating function @f@
            -> CVector a      -- ^ initial vector (of length @m@)
            -> CVector Int    -- ^ index vector (of length @n1@)
            -> CVector b      -- ^ value vector (of length @n2@)
            -> CVector a
accumulate_ = G.accumulate_
{-# INLINE accumulate_ #-}

unsafeAccum :: (a -> b -> a) -> CVector a -> [(Int,b)] -> CVector a
unsafeAccum = G.unsafeAccum
{-# INLINE unsafeAccum #-}

unsafeAccumulate :: (a -> b -> a) -> CVector a -> CVector (Int,b) -> CVector a
unsafeAccumulate = G.unsafeAccumulate
{-# INLINE unsafeAccumulate #-}

unsafeAccumulate_
  :: (a -> b -> a) -> CVector a -> CVector Int -> CVector b -> CVector a
unsafeAccumulate_ = G.unsafeAccumulate_
{-# INLINE unsafeAccumulate_ #-}

--
-- Permutations
--

reverse :: CVector a -> CVector a
reverse = G.reverse
{-# INLINE reverse #-}

backpermute :: CVector a -> CVector Int -> CVector a
backpermute = G.backpermute
{-# INLINE backpermute #-}

unsafeBackpermute :: CVector a -> CVector Int -> CVector a
unsafeBackpermute = G.unsafeBackpermute
{-# INLINE unsafeBackpermute #-}

--
-- Safe destructive updates
--

modify :: (forall s. CMVector s a -> ST s ()) -> CVector a -> CVector a
modify p = G.modify p
{-# INLINE modify #-}

--
-- Indexing
--

indexed :: CVector a -> CVector (Int,a)
indexed = G.indexed
{-# INLINE indexed #-}

--
-- Mapping
--

map :: (a -> b) -> CVector a -> CVector b
map = G.map
{-# INLINE map #-}

imap :: (Int -> a -> b) -> CVector a -> CVector b
imap = G.imap
{-# INLINE imap #-}

concatMap :: (a -> CVector b) -> CVector a -> CVector b
concatMap = G.concatMap
{-# INLINE concatMap #-}

--
-- Monadic mapping
--

mapM :: Monad m => (a -> m b) -> CVector a -> m (CVector b)
mapM = G.mapM
{-# INLINE mapM #-}

mapM_ :: Monad m => (a -> m b) -> CVector a -> m ()
mapM_ = G.mapM_
{-# INLINE mapM_ #-}

forM :: Monad m => CVector a -> (a -> m b) -> m (CVector b)
forM = G.forM
{-# INLINE forM #-}

forM_ :: Monad m => CVector a -> (a -> m b) -> m ()
forM_ = G.forM_
{-# INLINE forM_ #-}

--
-- Zipping
--

zipWith :: (a -> b -> c) -> CVector a -> CVector b -> CVector c
zipWith = G.zipWith
{-# INLINE zipWith #-}

zipWith3 :: (a -> b -> c -> d) -> CVector a -> CVector b -> CVector c -> CVector d
zipWith3 = G.zipWith3
{-# INLINE zipWith3 #-}

zipWith4
  :: (a -> b -> c -> d -> e)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
zipWith4 = G.zipWith4
{-# INLINE zipWith4 #-}

zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
         -> CVector f
zipWith5 = G.zipWith5
{-# INLINE zipWith5 #-}

zipWith6
  :: (a -> b -> c -> d -> e -> f -> g)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector f -> CVector g
zipWith6 = G.zipWith6
{-# INLINE zipWith6 #-}

izipWith :: (Int -> a -> b -> c) -> CVector a -> CVector b -> CVector c
izipWith = G.izipWith
{-# INLINE izipWith #-}

izipWith3
  :: (Int -> a -> b -> c -> d) -> CVector a -> CVector b -> CVector c -> CVector d
izipWith3 = G.izipWith3
{-# INLINE izipWith3 #-}

izipWith4 :: (Int -> a -> b -> c -> d -> e)
          -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
izipWith4 = G.izipWith4
{-# INLINE izipWith4 #-}

izipWith5
  :: (Int -> a -> b -> c -> d -> e -> f)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e -> CVector f
izipWith5 = G.izipWith5
{-# INLINE izipWith5 #-}

izipWith6
  :: (Int -> a -> b -> c -> d -> e -> f -> g)
  -> CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector f -> CVector g
izipWith6 = G.izipWith6
{-# INLINE izipWith6 #-}

zip :: CVector a -> CVector b -> CVector (a, b)
zip = G.zip
{-# INLINE zip #-}

zip3 :: CVector a -> CVector b -> CVector c -> CVector (a, b, c)
zip3 = G.zip3
{-# INLINE zip3 #-}

zip4 :: CVector a -> CVector b -> CVector c -> CVector d -> CVector (a, b, c, d)
zip4 = G.zip4
{-# INLINE zip4 #-}

zip5
  :: CVector a -> CVector b -> CVector c -> CVector d -> CVector e
  -> CVector (a, b, c, d, e)
zip5 = G.zip5
{-# INLINE zip5 #-}

zip6
  :: CVector a -> CVector b -> CVector c -> CVector d -> CVector e -> CVector f
  -> CVector (a, b, c, d, e, f)
zip6 = G.zip6
{-# INLINE zip6 #-}

--
-- Unzipping
--

unzip :: CVector (a, b) -> (CVector a, CVector b)
unzip = G.unzip
{-# INLINE unzip #-}

unzip3 :: CVector (a, b, c) -> (CVector a, CVector b, CVector c)
unzip3 = G.unzip3
{-# INLINE unzip3 #-}

unzip4 :: CVector (a, b, c, d) -> (CVector a, CVector b, CVector c, CVector d)
unzip4 = G.unzip4
{-# INLINE unzip4 #-}

unzip5
  :: CVector (a, b, c, d, e)
  -> (CVector a, CVector b, CVector c, CVector d, CVector e)
unzip5 = G.unzip5
{-# INLINE unzip5 #-}

unzip6
  :: CVector (a, b, c, d, e, f)
  -> (CVector a, CVector b, CVector c, CVector d, CVector e, CVector f)
unzip6 = G.unzip6
{-# INLINE unzip6 #-}

--
-- Monadic zipping
--

zipWithM :: Monad m => (a -> b -> m c) -> CVector a -> CVector b -> m (CVector c)
zipWithM = G.zipWithM
{-# INLINE zipWithM #-}

zipWithM_ :: Monad m => (a -> b -> m c) -> CVector a -> CVector b -> m ()
zipWithM_ = G.zipWithM_
{-# INLINE zipWithM_ #-}

--
-- Filtering
--

filter :: (a -> Bool) -> CVector a -> CVector a
filter = G.filter
{-# INLINE filter #-}

ifilter :: (Int -> a -> Bool) -> CVector a -> CVector a
ifilter = G.ifilter
{-# INLINE ifilter #-}

filterM :: Monad m => (a -> m Bool) -> CVector a -> m (CVector a)
filterM = G.filterM
{-# INLINE filterM #-}

takeWhile :: (a -> Bool) -> CVector a -> CVector a
takeWhile = G.takeWhile
{-# INLINE takeWhile #-}

dropWhile :: (a -> Bool) -> CVector a -> CVector a
dropWhile = G.dropWhile
{-# INLINE dropWhile #-}

--
-- Parititioning
--

partition :: (a -> Bool) -> CVector a -> (CVector a, CVector a)
partition = G.partition
{-# INLINE partition #-}

unstablePartition :: (a -> Bool) -> CVector a -> (CVector a, CVector a)
unstablePartition = G.unstablePartition
{-# INLINE unstablePartition #-}

span :: (a -> Bool) -> CVector a -> (CVector a, CVector a)
span = G.span
{-# INLINE span #-}

break :: (a -> Bool) -> CVector a -> (CVector a, CVector a)
break = G.break
{-# INLINE break #-}

--
-- Searching
--

infix 4 `elem`
elem :: Eq a => a -> CVector a -> Bool
elem = G.elem
{-# INLINE elem #-}

infix 4 `notElem`
notElem :: Eq a => a -> CVector a -> Bool
notElem = G.notElem
{-# INLINE notElem #-}

find :: (a -> Bool) -> CVector a -> Maybe a
find = G.find
{-# INLINE find #-}

findIndex :: (a -> Bool) -> CVector a -> Maybe Int
findIndex = G.findIndex
{-# INLINE findIndex #-}

findIndices :: (a -> Bool) -> CVector a -> CVector Int
findIndices = G.findIndices
{-# INLINE findIndices #-}

elemIndex :: Eq a => a -> CVector a -> Maybe Int
elemIndex = G.elemIndex
{-# INLINE elemIndex #-}

elemIndices :: Eq a => a -> CVector a -> CVector Int
elemIndices = G.elemIndices
{-# INLINE elemIndices #-}

--
-- Folding
--

foldl :: (a -> b -> a) -> a -> CVector b -> a
foldl = G.foldl
{-# INLINE foldl #-}

foldl1 :: (a -> a -> a) -> CVector a -> a
foldl1 = G.foldl1
{-# INLINE foldl1 #-}

foldl' :: (a -> b -> a) -> a -> CVector b -> a
foldl' = G.foldl'
{-# INLINE foldl' #-}

foldl1' :: (a -> a -> a) -> CVector a -> a
foldl1' = G.foldl1'
{-# INLINE foldl1' #-}

foldr :: (a -> b -> b) -> b -> CVector a -> b
foldr = G.foldr
{-# INLINE foldr #-}

foldr1 :: (a -> a -> a) -> CVector a -> a
foldr1 = G.foldr1
{-# INLINE foldr1 #-}

foldr' :: (a -> b -> b) -> b -> CVector a -> b
foldr' = G.foldr'
{-# INLINE foldr' #-}

foldr1' :: (a -> a -> a) -> CVector a -> a
foldr1' = G.foldr1'
{-# INLINE foldr1' #-}

ifoldl :: (a -> Int -> b -> a) -> a -> CVector b -> a
ifoldl = G.ifoldl
{-# INLINE ifoldl #-}

ifoldl' :: (a -> Int -> b -> a) -> a -> CVector b -> a
ifoldl' = G.ifoldl'
{-# INLINE ifoldl' #-}

ifoldr :: (Int -> a -> b -> b) -> b -> CVector a -> b
ifoldr = G.ifoldr
{-# INLINE ifoldr #-}

ifoldr' :: (Int -> a -> b -> b) -> b -> CVector a -> b
ifoldr' = G.ifoldr'
{-# INLINE ifoldr' #-}

--
-- Specialised folds
--

all :: (a -> Bool) -> CVector a -> Bool
all = G.all
{-# INLINE all #-}

any :: (a -> Bool) -> CVector a -> Bool
any = G.any
{-# INLINE any #-}

and :: CVector Bool -> Bool
and = G.and
{-# INLINE and #-}

or :: CVector Bool -> Bool
or = G.or
{-# INLINE or #-}

sum :: Num a => CVector a -> a
sum = G.sum
{-# INLINE sum #-}

product :: Num a => CVector a -> a
product = G.product
{-# INLINE product #-}

maximum :: Ord a => CVector a -> a
maximum = G.maximum
{-# INLINE maximum #-}

maximumBy :: (a -> a -> Ordering) -> CVector a -> a
maximumBy = G.maximumBy
{-# INLINE maximumBy #-}

minimum :: Ord a => CVector a -> a
minimum = G.minimum
{-# INLINE minimum #-}

minimumBy :: (a -> a -> Ordering) -> CVector a -> a
minimumBy = G.minimumBy
{-# INLINE minimumBy #-}

maxIndex :: Ord a => CVector a -> Int
maxIndex = G.maxIndex
{-# INLINE maxIndex #-}

maxIndexBy :: (a -> a -> Ordering) -> CVector a -> Int
maxIndexBy = G.maxIndexBy
{-# INLINE maxIndexBy #-}

minIndex :: Ord a => CVector a -> Int
minIndex = G.minIndex
{-# INLINE minIndex #-}

minIndexBy :: (a -> a -> Ordering) -> CVector a -> Int
minIndexBy = G.minIndexBy
{-# INLINE minIndexBy #-}

--
-- Monadic folds
--

foldM :: Monad m => (a -> b -> m a) -> a -> CVector b -> m a
foldM = G.foldM
{-# INLINE foldM #-}

fold1M :: Monad m => (a -> a -> m a) -> CVector a -> m a
fold1M = G.fold1M
{-# INLINE fold1M #-}

foldM' :: Monad m => (a -> b -> m a) -> a -> CVector b -> m a
foldM' = G.foldM'
{-# INLINE foldM' #-}

fold1M' :: Monad m => (a -> a -> m a) -> CVector a -> m a
fold1M' = G.fold1M'
{-# INLINE fold1M' #-}

foldM_ :: Monad m => (a -> b -> m a) -> a -> CVector b -> m ()
foldM_ = G.foldM_
{-# INLINE foldM_ #-}

fold1M_ :: Monad m => (a -> a -> m a) -> CVector a -> m ()
fold1M_ = G.fold1M_
{-# INLINE fold1M_ #-}

foldM'_ :: Monad m => (a -> b -> m a) -> a -> CVector b -> m ()
foldM'_ = G.foldM'_
{-# INLINE foldM'_ #-}

fold1M'_ :: Monad m => (a -> a -> m a) -> CVector a -> m ()
fold1M'_ = G.fold1M'_
{-# INLINE fold1M'_ #-}

--
-- Monadic sequencing
--

sequence :: Monad m => CVector (m a) -> m (CVector a)
sequence = G.sequence
{-# INLINE sequence #-}

sequence_ :: Monad m => CVector (m a) -> m ()
sequence_ = G.sequence_
{-# INLINE sequence_ #-}

--
-- Prefix sums (scans)
--

prescanl :: (a -> b -> a) -> a -> CVector b -> CVector a
prescanl = G.prescanl
{-# INLINE prescanl #-}

prescanl' :: (a -> b -> a) -> a -> CVector b -> CVector a
prescanl' = G.prescanl'
{-# INLINE prescanl' #-}

postscanl :: (a -> b -> a) -> a -> CVector b -> CVector a
postscanl = G.postscanl
{-# INLINE postscanl #-}

postscanl' :: (a -> b -> a) -> a -> CVector b -> CVector a
postscanl' = G.postscanl'
{-# INLINE postscanl' #-}

scanl :: (a -> b -> a) -> a -> CVector b -> CVector a
scanl = G.scanl
{-# INLINE scanl #-}

scanl' :: (a -> b -> a) -> a -> CVector b -> CVector a
scanl' = G.scanl'
{-# INLINE scanl' #-}

scanl1 :: (a -> a -> a) -> CVector a -> CVector a
scanl1 = G.scanl1
{-# INLINE scanl1 #-}

scanl1' :: (a -> a -> a) -> CVector a -> CVector a
scanl1' = G.scanl1'
{-# INLINE scanl1' #-}

prescanr :: (a -> b -> b) -> b -> CVector a -> CVector b
prescanr = G.prescanr
{-# INLINE prescanr #-}

prescanr' :: (a -> b -> b) -> b -> CVector a -> CVector b
prescanr' = G.prescanr'
{-# INLINE prescanr' #-}

postscanr :: (a -> b -> b) -> b -> CVector a -> CVector b
postscanr = G.postscanr
{-# INLINE postscanr #-}

postscanr' :: (a -> b -> b) -> b -> CVector a -> CVector b
postscanr' = G.postscanr'
{-# INLINE postscanr' #-}

scanr :: (a -> b -> b) -> b -> CVector a -> CVector b
scanr = G.scanr
{-# INLINE scanr #-}

scanr' :: (a -> b -> b) -> b -> CVector a -> CVector b
scanr' = G.scanr'
{-# INLINE scanr' #-}

scanr1 :: (a -> a -> a) -> CVector a -> CVector a
scanr1 = G.scanr1
{-# INLINE scanr1 #-}

scanr1' :: (a -> a -> a) -> CVector a -> CVector a
scanr1' = G.scanr1'
{-# INLINE scanr1' #-}

--
-- Conversions - Lists
--

toList :: CVector a -> [a]
toList = G.toList
{-# INLINE toList #-}

fromList :: [a] -> CVector a
fromList = G.fromList
{-# INLINE fromList #-}

fromListN :: Int -> [a] -> CVector a
fromListN = G.fromListN
{-# INLINE fromListN #-}

--
-- Conversions - Mutable vectors
--

unsafeFreeze :: (PrimMonad m) => CMVector (PrimState m) a -> m (CVector a)
unsafeFreeze = G.unsafeFreeze
{-# INLINE unsafeFreeze #-}

unsafeThaw :: (PrimMonad m) => CVector a -> m (CMVector (PrimState m) a)
unsafeThaw = G.unsafeThaw
{-# INLINE unsafeThaw #-}

thaw :: (PrimMonad m) => CVector a -> m (CMVector (PrimState m) a)
thaw = G.thaw
{-# INLINE thaw #-}

freeze :: (PrimMonad m) => CMVector (PrimState m) a -> m (CVector a)
freeze = G.freeze
{-# INLINE freeze #-}

unsafeCopy :: (PrimMonad m) => CMVector (PrimState m) a -> CVector a -> m ()
unsafeCopy = G.unsafeCopy
{-# INLINE unsafeCopy #-}

copy :: (PrimMonad m) => CMVector (PrimState m) a -> CVector a -> m ()
copy = G.copy
{-# INLINE copy #-}
