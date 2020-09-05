module Main (main) where

import System.Environment
import AssocList
import BSTree
import AVLTree

main = do
   [file,dType] <- getArgs
   input <- readFile file
   let pairs = zip (words input) (repeat 1)
       result = case dType of
          "AL"   -> toList $foldl (pUpdate (+)) 
             (empty :: AssocList String Int) pairs
          "BST"  -> toList $ foldl (pUpdate (+)) 
             (empty :: BSTree String Int) pairs
          "AVLT" -> toList $ foldl (pUpdate (+)) 
             (empty :: AVLTree String Int) pairs
   print result
