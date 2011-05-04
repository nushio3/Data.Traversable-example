{-# OPTIONS -Wall #-}
import Data.Foldable
import Data.Functor
import Data.Traversable
import Prelude hiding (mapM,fmap,foldl1,foldr1)
import Tree

main :: IO ()
main = do
  let t = Node "sazae" (Node "katsuo" Empty Empty) (Node "wakame" Empty Empty)
  Print t
  _ <- mapM (putStrLn) t
  Print $ mapM (const ["M", "F"]) t
  print $ fmap (length) t
  print $ foldMap (:[]) t
  print $ foldMap (\x -> [x, x++"'s cat"]) t
  putStrLn $ foldl1 (\x y -> " <"++x++" "++y++"> ") t
  putStrLn $ foldr1 (\x y -> " <"++x++" "++y++"> ") t
  return ()
      
