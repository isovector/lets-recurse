{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module MyPrelude where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Nat = Z | S Nat

makeBaseFunctor ''Nat


myEnumFrom :: a -> [a]
myEnumFrom = undefined

myEnumFromThen :: a -> a -> [a]
myEnumFromThen = undefined

myEnumFromTo :: a -> a -> [a]
myEnumFromTo = undefined

myEnumFromThenTo :: a -> a -> a -> [a]
myEnumFromThenTo = undefined

myMconcat :: [a] -> a
myMconcat = undefined

myMapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
myMapM_ = undefined

mySequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
mySequence_ = undefined

myFoldMap :: Monoid m => (a -> m) -> t a -> m
myFoldMap = undefined

myFoldr :: (a -> b -> b) -> b -> t a -> b
myFoldr = undefined

myFoldl :: (b -> a -> b) -> b -> t a -> b
myFoldl = undefined

myFoldr1 :: (a -> a -> a) -> t a -> a
myFoldr1 = undefined

myFoldl1 :: (a -> a -> a) -> t a -> a
myFoldl1 = undefined

myElem :: Eq a => a -> t a -> Bool
myElem = undefined

myMaximum ::  Ord a => t a -> a
myMaximum = undefined

myMinimum ::  Ord a => t a -> a
myMinimum = undefined

mySum :: Num a => t a -> a
mySum = undefined

myProduct :: Num a => t a -> a
myProduct = undefined

myTraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
myTraverse = undefined

mySequenceA :: Applicative f => t (f a) -> f (t a)
mySequenceA = undefined

myMapM :: Monad m => (a -> m b) -> t a -> m (t b)
myMapM = undefined

mySequence :: Monad m => t (m a) -> m (t a)
mySequence = undefined

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil = undefined

myMap :: (a -> b) -> [a] -> [b]
myMap = undefined

myConcat :: [a] -> [a] -> [a]
myConcat = undefined

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined

myHead :: [a] -> a
myHead =
  let alg Nil = undefined
      alg (Cons a _) = a
   in cata alg

myLast :: [a] -> a
myLast =
  let alg Nil = Right ()
      alg (Cons a as) = as >> Left a
   in either id undefined . cata alg

myInit :: [a] -> [a]
myInit =
  let alg Nil = Nothing
      alg (Cons a as) =
        case as of
          Nothing -> Just []
          Just _ -> (a :) <$> as
   in maybe undefined id . cata alg

myNull :: [a] -> Bool
myNull =
  let alg Nil = True
      alg (Cons _ _) = False
   in cata alg

myLength :: [a] -> Int
myLength =
  let alg Nil = 0
      alg (Cons _ n) = n + 1
   in cata alg

myIndex :: [a] -> Nat -> a
myIndex as =
  let alg ZF = as
      alg (SF a) = tail a
   in head . cata alg

myReverse :: [a] -> [a]
myReverse =
  let alg Nil = []
      alg (Cons a b) = b ++ [a]
   in cata alg

myAnd :: [Bool] -> Bool
myAnd =
  let alg Nil = True
      alg (Cons a b) = a && b
   in cata alg

myAll :: (a -> Bool) -> [a] -> Bool
myAll f =
  let alg Nil = True
      alg (Cons a b) = f a && b
   in cata alg

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f =
  let alg Nil = []
      alg (Cons a bs) = f a ++ bs
   in cata alg

myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl = undefined

myScanl1 :: (a -> a -> a) -> [a] -> [a]
myScanl1 = undefined

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr = undefined

myScanr1 :: (a -> a -> a) -> [a] -> [a]
myScanr1 = undefined

myIterate :: (a -> a) -> a -> [a]
myIterate f =
  let coalg a = Cons a (f a)
   in ana coalg

myRepeat :: a -> [a]
myRepeat =
  let coalg a = Cons a a
   in ana coalg

myReplicate :: a -> Nat -> [a]
myReplicate a =
  let alg ZF = []
      alg (SF as) = a : as
   in cata alg

myCycle :: [a] -> [a]
myCycle = undefined

myTake :: Int -> [a] -> [a]
myTake = undefined

myDrop :: Int -> [a] -> [a]
myDrop = undefined

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt = undefined

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile = undefined

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile = undefined

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan = undefined

myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak = undefined

myNotElem :: (Foldable t, Eq a) => a -> t a -> Bool
myNotElem = undefined

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup a =
  let alg Nil = Nothing
      alg (Cons (a', b) x) =
        if a == a'
           then Just b
           else x
   in cata alg

myZip :: [a] -> [b] -> [(a, b)]
myZip = undefined

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 = undefined

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith = undefined

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 = undefined

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = undefined

myUnzip3 :: [(a, b, c)] -> ([a], [b], [c])
myUnzip3 = undefined

myLines :: String -> [String]
myLines = undefined

myWords :: String -> [String]
myWords = undefined

myUnlines :: [String] -> String
myUnlines = undefined

myUnwords :: [String] -> String
myUnwords = undefined

