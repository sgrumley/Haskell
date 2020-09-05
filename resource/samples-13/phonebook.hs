-- phonebook

module Main (main) where

import System.IO
import AssocList -- could be BSTree or AVLTree

type PhoneBook = AssocList String String

main = loop empty

loop :: PhoneBook -> IO ()
loop book = do
   putStr "? "
   hFlush stdout
   command <- getLine
   case words command of
      ["quit"] -> 
         return ()
      ["add", name, number] -> 
         loop (put book name number)
      ["find", name] -> do
         case get name book of 
            Nothing ->
               putStrLn "Not found."
            Just number ->
               putStrLn number
         loop book
      _ -> do
         putStrLn "Bad command."
         loop book
