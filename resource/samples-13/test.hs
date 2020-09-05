module Main (main) where

import BSTree

main = do
   let d :: BSTree String Int
       d = fromList [("a", 3), ("b", 5)]
       d' = put d "k" 3
   print $ toList d'
