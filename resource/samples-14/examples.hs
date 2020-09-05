import Prelude hiding (putStr, traverse)
import Data.Char

import BSTree
import Summer

t1 :: BSTree String Int
t1 = fromList [("Brandy", 1), ("Andy", 2), ("Candy",  3)] 

t2 :: BSTree Int Double
t2 = fromList [( 1, 1.0 ), (10, 2.0 ), ( 5, 0.25),
               ( 7, 2.5 ), ( 6, 4.0 ), (11, 3.0 )] 
                
putStr :: String -> IO ()
putStr cs = case cs of
   []     -> return ()
   c : cs -> do putChar c
                putStr cs

seq1 :: IO ()
seq1 = getChar >>= (\c -> putChar c >> putChar c)

seq2 :: IO ()
seq2 = do c <- getChar
          putChar c
          putChar c

seq3 :: IO ()
seq3 = do c <- getChar
          let c' = toUpper c
              c'' = succ c'
          putChar c'
          putChar c''
          
