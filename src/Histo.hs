{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Histo where

import Debug.Trace
import Data.Array
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Comonad.Cofree
import Control.Comonad


showTrace :: Show a => a -> a
showTrace = trace =<< show

data Nat = Z | S Nat
  deriving Show

makeBaseFunctor ''Nat

instance Show a => Show (NatF (Cofree NatF a)) where
  show ZF = "ZF"
  show (SF (x :< ys)) = "(SF " ++ show x ++ " :< " ++ show ys ++ ")"


fib :: Nat -> Integer
fib = histo alg
  where
    alg (showTrace -> SF (x :< SF (y :< _))) = x + y
    alg (showTrace -> _) = 1


data Table a = Table a [a] [a] [[a]] [[a]]
  deriving (Show, Functor)

makeBaseFunctor ''Table


focus :: Table a -> a
focus (Table a _ _ _ _) = a

left :: Table a -> Table a
left tab@(Table _ [] _ _ _) = tab
left (Table sel (l:ls) rs ts bs) = Table l ls (sel:rs) ts bs

right :: Table a -> Table a
right tab@(Table _ _ [] _ _) = tab
right (Table sel ls (r:rs) ts bs) = Table r (sel:ls) rs ts bs

up :: Table a -> Table a
up tab@(Table _ _ _ [] _) = tab
up (Table sel ls rs (t:ts) bs) = Table sel' ls' rs' ts (b:bs)
  where
    (ls',(sel':rs')) = splitAt (length ls) t
    b = ls ++ (sel:rs)

down :: Table a -> Table a
down tab@(Table _ _ _ _ []) = tab
down (Table sel ls rs ts (b:bs)) = Table sel' ls' rs' (t:ts) bs
  where
    (ls',(sel':rs')) = splitAt (length ls) b
    t = ls ++ (sel:rs)

tableToList :: Table a -> [[a]]
tableToList (Table sel ls rs ts bs) = (reverse ts) ++ [ls ++ (sel:rs)] ++ bs

listToTable :: [[a]] -> Table a
listToTable [] = error "cannot make empty table"
listToTable ([]:_) = error "cannot make empty table"
listToTable ((t:tr):ts) = Table t [] tr [] ts


-- solveMyShit :: Ord a => Table a -> Int
-- solveMyShit = snd . histo alg
--   where
--     alg (TableF ((a, v) :< t) _ _ _ _)  =
--       let l = undefined
--        in (a, v)

-- Recursive t => (Base t (Cofree (Base t) a) -> a) -> t -> a

-- 0 1 0 5 1 0
-- 1 0 1 1 2 0
-- 5 4 3 2 1 0
-- 6 2 3 4 5 6


