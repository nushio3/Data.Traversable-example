{-# OPTIONS -Wall #-}
import Control.Applicative
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Traversable
import Prelude hiding (mapM,sequence,fmap,foldr, foldl,foldl1,foldr1)

data Tree a = Empty | Node a (Tree a) (Tree a) 

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show (Node x left right) = "(" ++ show x ++ show left ++ show right ++ ")"

instance Functor Tree where
  fmap = fmapDefault
instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse _ Empty               = pure Empty
  traverse up (Node x left right) = 
    Node <$> up x <*> traverse up left <*> traverse up right

