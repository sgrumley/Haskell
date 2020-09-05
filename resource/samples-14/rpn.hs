module Main (main) where

import System.IO
import Control.Exception

type Stack = [Double]

main :: IO ()
main = rpn []

rpn :: Stack -> IO ()
rpn stack = do
   putStr "? "
   hFlush stdout
   command <- getLine
   let op1 f = case stack of
          []     -> insuffError
          x : xs -> rpn (f x : xs)
       op2 f = case stack of
          []           -> insuffError
          [x]          -> insuffError
          x1 : x2 : xs -> rpn (f x2 x1 : xs)
       insuffError = do
          putStrLn "Insufficient items on stack."
          rpn stack
       readNum = do
          x <- readIO command
          rpn (x : stack)
       handler :: IOException -> IO ()
       handler _ = do
         putStrLn "Can't understand that."
         rpn stack 
   case command of
      "q"    -> return ()
      "+"    -> op2 (+)
      "-"    -> op2 (-)
      "*"    -> op2 (*)
      "/"    -> op2 (/)
      "1/x"  -> op1 recip
      "sqrt" -> op1 sqrt
      "."    -> case stack of
         []     -> rpn stack
         _ : xs -> rpn xs
      "?"    -> 
         case stack of
            []     -> insuffError
            x : xs -> do 
               print $ head stack
               rpn stack
      "??"   -> do
         mapM_ print $ reverse stack
         rpn stack 
      _      -> 
         catch readNum handler
