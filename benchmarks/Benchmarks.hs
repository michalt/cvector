{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST.Strict

import Criterion.Main

import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as VectorU

import qualified Data.CVector as CVector
import qualified Data.CVector.Mutable as CMVector
import qualified Data.CVector.Unboxed as CVectorU
import qualified Data.CVector.Unboxed.Mutable as CMVectorU

generate :: (Int -> a -> a) -> a -> Int -> a
generate f acc limit = go 0 acc
  where
    go !i !a | i < limit = go (i + 1) (f i a)
             | otherwise = a
{-# INLINE generate #-}

generateM :: (a -> Int -> ST s a) -> (ST s a) -> Int -> ST s a
generateM f new limit = new >>= go 0
  where
    go !i !a | i < limit = f a i >>= go (i + 1)
             | otherwise = return a
{-# INLINE generateM #-}

main :: IO ()
main = do
  defaultMain
    [ bench "Data.Vector.cons  10^3 elements"                      $ whnf testVector     1000
    , bench "Data.Vector.Unboxed.cons  10^3 elements"              $ whnf testVectorU    1000
    , bench "Data.List.(:)  10^5 elements"                         $ whnf testList     100000
    , bench "Data.CVector.Mutable.pushBack  10^5 elements"         $ whnf testCVector  100000
    , bench "Data.CVector.Unboxed.Mutable.pushBack  10^5 elements" $ whnf testCVectorU 100000
    ]

testVector :: Int -> Vector.Vector Int
testVector = generate Vector.cons Vector.empty
{-# INLINE testVector #-}

testVectorU :: Int -> VectorU.Vector Int
testVectorU = generate VectorU.cons VectorU.empty
{-# INLINE testVectorU #-}

testList :: Int -> [Int]
testList = generate (:) []
{-# INLINE testList #-}

testCVector :: Int -> CVector.CVector Int
testCVector l = runST
  (generateM CMVector.pushBack (CMVector.new 0) l >>= CVector.unsafeFreeze)

testCVectorU :: Int -> CVectorU.CVector Int
testCVectorU l = runST
  (generateM CMVectorU.pushBack (CMVectorU.new 0) l >>= CVectorU.unsafeFreeze)

sanity :: IO ()
sanity = do
  print $
    reverse (Vector.toList (testVector 100)) == CVector.toList (testCVector 100)
  print $
    reverse (VectorU.toList (testVectorU 100)) == CVectorU.toList (testCVectorU 100)
