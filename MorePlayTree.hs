{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS -Wall #-}
import Data.Foldable
import Data.Functor
import Data.Traversable
import Prelude hiding (mapM,sequence,fmap,foldr, foldl,foldl1,foldr1)

data Tree a = Empty | Node a (Tree a) (Tree a) 
            deriving (Functor, Foldable, Traversable)
              
instance (Show a) => Show (Tree a) where
  show Empty = ""
  show (Node x left right) = "(" ++ show x ++ show left ++ show right ++ ")"
