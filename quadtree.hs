-- ppeeefpffeefe + pefepeefe = ppeeefffpeefe


module Main (main) where

import Control.Monad (forM_)

data QuadTree =
    Node QuadTree QuadTree QuadTree QuadTree
    | Empty
    | Full
    deriving (Show, Eq)
  
   
  
parseQT :: String -> (QuadTree, String)
parseQT ('e' : cs) = (Empty, cs) 
parseQT ('f' : cs) = (Full, cs)
parseQT ('p' : cs) = 
    let (st1, cs1) = parseQT cs
        (st2, cs2) = parseQT cs1
        (st3, cs3) = parseQT cs2
        (st4, cs4) = parseQT cs3
    in (Node st1 st2 st3 st4, cs4)
parseQT "" = error "parseQT \"\""



main::IO()
main = interact rf

driver:: String -> String
driver s = show $ tree s []


rf :: String -> String
rf file =
    let strLines = lines file
        nTests = head strLines
        newList = drop 1 strLines --take 1 strLines
        
        firstCase = take 2 newList
        -- change testCase to return array of tupled QuadTree
        datas = testCase (read nTests) newList []
    --in show firstCase
    in printQuad datas []
    --in unlines $(take 2) strLines


testCase :: Int -> [String] -> [((QuadTree, String), (QuadTree, String))]-> [((QuadTree, String), (QuadTree, String))]
testCase test inputs outputs   | test == 0    = outputs 
                               | otherwise    =  
    let tcase = take 2 inputs
        newList = drop 2 inputs
        y = parseQT (head tcase)
        z = parseQT (last tcase)
        x = (y, z)
        --x = (head tcase, last tcase)
    in testCase (test -1) newList (outputs ++ [x])


tree:: String -> [QuadTree] -> [QuadTree]
tree t bs = 
        let (a, b) = parseQT t
        in if b == "" then bs 
                      else tree b (bs ++ [a])


printQuad :: [((QuadTree, String), (QuadTree, String))] -> [String] -> String
printQuad (x:xs) ys = printQuad xs (ys ++ [formatOne x])
printQuad [x] ys = unlines $ (ys ++ [formatOne x])
printQuad [] ys = unlines $ ys

formatOne :: ((QuadTree, String), (QuadTree, String)) -> String
formatOne ((s,x),(a, f)) = (show s) ++ " : " ++  x ++ " vs " ++ (show a) ++ " : " ++ f
-- 

-- printTuples :: [((QuadTree, String), (QuadTree, String))] -> [String] -> String
-- printTuples (x:xs) ys = printTuples xs (ys ++ [formatOne x])
-- printTuples [x] ys = unlines $ (ys ++ [formatOne x])
-- printTuples [] ys = unlines $ ys

-- formatOne :: (String, String) -> String
-- formatOne (s,x) = s ++ " : " ++  x
-- Read in how many test cases
-- Read in 2 lines n times
-- Read both string as trees
-- compare