-- ppeeefpffeefe + pefepeefe = ppeeefffpeefe


module Main (main) where


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

  
parseQTd :: String -> Int -> Int -> (Int, String)
parseQTd ('e' : cs) d v = (v, cs) 
parseQTd ('f' : cs) d v = (v + (1024 `div` (4 ^ d)) , cs)
parseQTd ('p' : cs) d v = 
    let (st1, cs1) = parseQTd cs (d + 1) v
        (st2, cs2) = parseQTd cs1 (d + 1) v
        (st3, cs3) = parseQTd cs2 (d + 1) v
        (st4, cs4) = parseQTd cs3 (d + 1) v
    in (st1 + st2 +  st3 + st4, cs4)


main::IO()
main = interact driver

driver:: String -> String
driver file =
    let strLines = lines file
        nTests = head strLines
        newList = drop 1 strLines --take 1 strLines
        
        firstCase = head newList
        --datas = parseQT firstCase
        --dataa = parseQTd firstCase 0 0 
        tcs = readFiler newList []
        -- change testCase to return array of tupled QuadTree
        --datas = testCase (read nTests) newList []
    --in show firstCase
    in (printQuad tcs [])
    --in unlines $(take 2) strLines

-- Read data into array of test cases
readFiler:: [String] -> [(String, String)] -> [(String, String)]
readFiler [] outputs = outputs
readFiler inputs outputs =
    let tcase = take 2 inputs
        newList = drop 2 inputs
        a = head tcase
        b = last tcase
        tc = (a, b)
    in readFiler newList (outputs ++ [tc])

cleanQuery :: (Int, String) -> Int
cleanQuery (val, discard) = val 

-- Single line the test case
printStruct :: (String, String) -> String
printStruct (t1, t2) = 
    let res1 = cleanQuery(parseQTd t1 0 0)
        res2 = cleanQuery(parseQTd t2 0 0)
        totalSum = res1 + res2
    in show("test case: " ++  t1 ++ "(" ++ (show res1) ++ ") + " ++ t2 ++ "(" ++ (show res2) ++ ") = " ++ (show(totalSum)))

-- Single line all test cases
printQuad :: [(String, String)] -> [String] -> String
printQuad (x:xs) ys = printQuad xs (ys ++ [printStruct x])
printQuad [x] ys = unlines $ (ys ++ [printStruct x])
printQuad [] ys = unlines $ ys



-- printQuad :: [((QuadTree, String), (QuadTree, String))] -> [String] -> String
-- printQuad (x:xs) ys = printQuad xs (ys ++ [formatOne x])
-- printQuad [x] ys = unlines $ (ys ++ [formatOne x])
-- printQuad [] ys = unlines $ ys

-- formatOne :: ((QuadTree, String), (QuadTree, String)) -> String
-- formatOne ((s,x),(a, f)) = (show s) ++ " : " ++  x ++ " vs " ++ (show a) ++ " : " ++ f



tree:: String -> [QuadTree] -> [QuadTree]
tree t bs = 
    let (a, b) = parseQT t
    in if b == "" then bs 
    else tree b (bs ++ [a])

-- rf :: String -> String
-- rf file =
--     let strLines = lines file
--         nTests = head strLines
--         newList = drop 1 strLines --take 1 strLines
        
--         firstCase = take 2 newList
--         -- change testCase to return array of tupled QuadTree
--         datas = testCase (read nTests) newList []
--     --in show firstCase
--     in printQuad datas []
--     --in unlines $(take 2) strLines


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




--traverseQT :: (QuadTree, String) -> depthCounter -> Sum

-- traverseQT :: (QuadTree, String) -> Int  -> Int -> Int
-- traverseQT (qt,s) dc sum =
--     let stringed = head s

--     in sparseQT s
    
-- 
-- string of children, depth, , value
-- sparseQT :: String -> Int -> Int -> Int
-- sparseQT ('e' : cs) d v = 0 
-- sparseQT ('f' : cs) d v = 10--(4 ^ d) `div` 1024
-- sparseQT ('p' : cs) d v = 
--     let aa = sparseQT cs (d + 1)  v --newLista = drop 1 cs
--         newListb = drop 1 cs
--         bb = sparseQT newListb (d + 1) v
--         newListc = drop 1 newListb
--         cc = sparseQT newListc (d + 1) v
--         newListd = drop 1 newListc
--         dd = sparseQT newListd (d + 1) v
--     in aa + bb + cc + dd

sparseQT :: String -> Int -> Int -> Int
sparseQT ('e' : cs) d v = v 
sparseQT ('f' : cs) d v = v + 10--(4 ^ d) `div` 1024
sparseQT ('p' : cs) d v = 
    let aa = sparseQT cs (d + 1)  v --newLista = drop 1 cs
        newListb = drop 1 cs
        bb = sparseQT newListb (d + 1) v
        newListc = drop 1 newListb
        cc = sparseQT newListc (d + 1) v
        newListd = drop 1 newListc
        dd = sparseQT newListd (d + 1) v
    in aa + bb + cc + dd

--Find way to get value of tree
-- 1024 / 4^depth = value of full
-- When traversing if node = full, call add to int
-- if node = p plus one to depth, if no p in the next 4, minus one from depth