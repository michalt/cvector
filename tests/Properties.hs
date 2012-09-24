{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Monad ( foldM )
import Control.Monad.ST ( ST, runST )

import Test.QuickCheck ( Arbitrary(..), Gen, choose )
import Test.Framework ( Test, defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Data.CVector.Generic ( CVector, Mutable, Vector )
import qualified Data.CVector.Generic as Generic
import Data.CVector.Generic.Mutable ( CMVector )
import qualified Data.CVector.Generic.Mutable as MGeneric

import qualified Data.CVector as CVector

import qualified Data.CVector.Unboxed as CVectorU

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Boxed CVector"

    [ testProperty "pFromToList"  (pFromToList boxed)
    , testProperty "pThawFreeze"  (pThawFreeze boxed)
    , testProperty "pPushBack"  (pPushBack boxed)
    , testProperty "pPopBack"  (pPopBack boxed)
    , testProperty "pPopBackOverwrite"  (pPopBackOverwrite boxed)
    ]

  , testGroup "Unboxed CVector"

    [ testProperty "pFromToList"  (pFromToList unboxed)
    , testProperty "pThawFreeze"  (pThawFreeze unboxed)
    , testProperty "pPushBack"  (pPushBack boxed)
    , testProperty "pPopBack"  (pPopBack boxed)
    ]

  ]

instance (Arbitrary a, Vector v a) => Arbitrary (CVector v a) where
  arbitrary = Generic.fromList <$> arbitrary
  shrink v = map Generic.fromList $ shrink (Generic.toList v)

boxed :: CVector.CVector Int
boxed = undefined

unboxed :: CVectorU.CVector Int
unboxed = undefined

pFromToList :: forall a. forall v. (Eq a, Vector v a) => CVector v a -> [a] -> Bool
pFromToList _ xs = Generic.toList (Generic.fromList xs :: CVector v a) == xs

pThawFreeze :: (Eq (v a), Vector v a) => CVector v a -> CVector v a -> Bool
pThawFreeze _ v = runST st
  where
    st :: forall s. ST s Bool
    st = do
      mv :: CMVector (Mutable v) s a <- Generic.thaw v
      v' :: CVector v a <- Generic.freeze mv
      return $! v == v'

pPushBack :: forall a. forall v. (Eq a, Vector v a)
  => CVector v a -> CVector v a -> [a] -> Bool
pPushBack _ vec ys = runST st
  where
    st :: forall s. ST s Bool
    st = do
      mvec :: CMVector (Mutable v) s a <- Generic.thaw vec
      mvec' <- foldM (\mv a -> MGeneric.pushBack mv a) mvec ys
      vec' :: CVector v a <- Generic.freeze mvec'
      return $! Generic.toList vec' == (Generic.toList vec) ++ ys

pPopBack :: forall a. forall v. (Eq a, Vector v a)
  => CVector v a -> CVector v a -> Gen Bool
pPopBack = pPopBack_ MGeneric.popBack

pPopBackOverwrite :: forall a. forall v. (Eq a, Vector v a)
  => CVector v a -> CVector v a -> Gen Bool
pPopBackOverwrite = pPopBack_ MGeneric.popBackOverwrite

pPopBack_ :: forall a. forall v. (Eq a, Vector v a)
  => (forall s. CMVector (Mutable v) s a -> ST s (CMVector (Mutable v) s a))
  -> CVector v a
  -> CVector v a
  -> Gen Bool
pPopBack_ pop _ vec = choose (0, Generic.length vec) >>= \i -> return (runST (st i))
  where
    st :: forall s. Int -> ST s Bool
    st i = do
      mvec :: CMVector (Mutable v) s a <- Generic.thaw vec
      mvec' <- foldM (const . pop) mvec (replicate i ())
      vec' :: CVector v a <- Generic.freeze mvec'
      return $! Generic.toList vec' == take (MGeneric.length mvec - i)
                                            (Generic.toList vec)
