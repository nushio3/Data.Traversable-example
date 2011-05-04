{-# OPTIONS -Wall #-}
import Data.Foldable
import Data.Functor
import Data.Monoid
import Data.Traversable
import Prelude hiding (mapM,fmap,foldl1,foldr1)
import Tree

age :: String -> Int
age s = if s=="sazae" then 24 else if s=="katsuo" then 11 else 9

main :: IO ()
main = do
  let t = Node "sazae" (Node "katsuo" Empty Empty) (Node "wakame" Empty Empty)
  print t
  _ <- mapM (putStrLn) t
  print $ mapM (const ["M", "F"]) t
  print $ fmap (length) t
  print $ foldMap (:[]) t
  print $ foldMap (\x -> [x, x++"'s cat"]) t
  print $ foldMap (Product . age) t
  putStrLn $ foldl1 (\x y -> " <"++x++" "++y++"> ") t
  putStrLn $ foldr1 (\x y -> " <"++x++" "++y++"> ") t
  return ()
      
