module MyPrelude where

import Data.Functor.Foldable


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
myHead = undefined

myLast :: [a] -> a
myLast = undefined

myTail :: [a] -> [a]
myTail = undefined

myInit :: [a] -> [a]
myInit = undefined

myNull :: Foldable t => t a -> Bool
myNull = undefined

myLength :: Foldable t => t a -> Int
myLength = undefined

myIndex :: [a] -> Int -> a
myIndex = undefined

myReverse :: [a] -> [a]
myReverse = undefined

myAnd :: Foldable t => t Bool -> Bool
myAnd = undefined

myOr :: Foldable t => t Bool -> Bool
myOr = undefined

myAny :: Foldable t => (a -> Bool) -> t a -> Bool
myAny = undefined

myAll :: Foldable t => (a -> Bool) -> t a -> Bool
myAll = undefined

myConcatMap :: Foldable t => (a -> [b]) -> t a -> [b]
myConcatMap = undefined

myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl = undefined

myScanl1 :: (a -> a -> a) -> [a] -> [a]
myScanl1 = undefined

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr = undefined

myScanr1 :: (a -> a -> a) -> [a] -> [a]
myScanr1 = undefined

myIterate :: (a -> a) -> a -> [a]
myIterate = undefined

myRepeat :: a -> [a]
myRepeat = undefined

myReplicate :: Int -> a -> [a]
myReplicate = undefined

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
myLookup = undefined

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

