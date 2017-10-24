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


myEnumFrom :: Enum a => a -> [a]
myEnumFrom =
  let coalg a = Cons a $ succ a
   in ana coalg

myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo from to =
  let coalg a =
        if fromEnum a > fromEnum to
           then Nil
           else Cons a $ succ a
   in ana coalg from

myMconcat :: Monoid a => [a] -> a
myMconcat =
  let alg Nil = mempty
      alg (Cons a ms) = mappend a ms
   in cata alg

myFoldMap :: Monoid m => (a -> m) -> [a] -> m
myFoldMap f =
  let alg Nil        = mempty
      alg (Cons a b) = mappend (f a) b
   in cata alg

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b' =
  let alg Nil = b'
      alg (Cons a b) = f a b
   in cata alg

myFoldl :: (b -> a -> b) -> b -> t a -> b
myFoldl = undefined

myFoldr1 :: (a -> a -> a) -> t a -> a
myFoldr1 = undefined

myFoldl1 :: (a -> a -> a) -> t a -> a
myFoldl1 = undefined

myElem :: Eq a => a -> [a] -> Bool
myElem a =
  let alg Nil = False
      alg (Cons a' x) = x || a == a'
   in cata alg

myMaximum ::  (Bounded a, Ord a) => [a] -> a
myMaximum =
  let alg Nil = minBound
      alg (Cons a m) = max a m
   in cata alg

mySum :: Num a => [a] -> a
mySum =
  let alg Nil = 0
      alg (Cons n ns) = n + ns
   in cata alg

myTraverse :: Applicative f => (a -> f b) -> [a] -> f [b]
myTraverse f =
  let alg Nil = pure []
      alg (Cons a fbs) = (:) <$> f a <*> fbs
   in cata alg

mySequenceA :: Applicative f => [f a] -> f [a]
mySequenceA =
  let alg Nil = pure []
      alg (Cons fa fas) = (:) <$> fa <*> fas
   in cata alg

mySequence :: Monad m => t (m a) -> m (t a)
mySequence = undefined

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil = undefined

myMap :: (a -> b) -> [a] -> [b]
myMap f =
  let alg Nil = []
      alg (Cons a bs) = f a : bs
  in cata alg

myConcat :: [a] -> [a] -> [a]
myConcat =
  let coalg ([], [])   = Nil
      coalg ([], y:ys) = Cons y ([], ys)
      coalg (x:xs, ys) = Cons x (xs, ys)
   in curry $ ana coalg

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f =
  let alg Nil = []
      alg (Cons a b) =
        if f a
           then a : b
           else b
   in cata alg

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
myCycle =
  let coalg (a : as) = Cons a (as ++ [a])
   in ana coalg

myTake :: [a] -> Nat -> [a]
myTake = (fst .) . mySplitAt

myDrop :: [a] -> Nat -> [a]
myDrop as =
  let alg ZF = as
      alg (SF (_:a')) = a'
      alg (SF [])     = []
   in cata alg

mySplitAt :: [a] -> Nat -> ([a], [a])
mySplitAt as =
  let alg ZF = ([], as)
      alg (SF (x, (a : as))) = (x ++ [a], as)
      alg (SF (x, []))       = (x, [])
   in cata alg

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile = undefined

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile = undefined

myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak f =
  let alg Nil = (False, ([], []))
      alg (Cons b (True,  (as, bs))) = (True, (as, bs ++ [b]))
      alg (Cons a (False, (as, bs))) =
        if not (f a)
           then (True, (as, bs ++ [a]))
           else (False, (as ++ [a], bs))
   -- TODO(sandy): disgusting af
   in snd . cata alg . reverse

myNotElem :: (Eq a) => a -> [a] -> Bool
myNotElem = (not .) . myElem

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup a =
  let alg Nil = Nothing
      alg (Cons (a', b) x) =
        if a == a'
           then Just b
           else x
   in cata alg

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f =
  let coalg (_, []) = Nil
      coalg ([], _) = Nil
      coalg (a:as, b:bs) = Cons (f a b) (as, bs)
   in curry $ ana coalg

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip =
  let alg Nil = ([], [])
      alg (Cons (a, b) (as, bs)) = (a:as, b:bs)
   in cata alg

myLines :: String -> [String]
myLines =
  let coalg "" = Nil
      coalg str =
        case myBreak (/= '\n') str of
          (as, []) -> Cons as ""
          (as, bs) -> Cons as $ tail bs
   in ana coalg

myUnlines :: [String] -> String
myUnlines =
  let alg Nil = ""
      alg (Cons a as) = a ++ "\n" ++ as
   in cata alg

