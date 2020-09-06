module Main (main) where 

main::IO()
main = interact driver

driver:: String -> String
driver file =
    let strLines = lines file
        newList = drop 1 strLines
        tcs = readFiler newList []
    in (printQuad tcs [])

-- Convert string input to value of black pixels    
parseQTtoValue :: String -> Int -> Int -> (Int, String)
parseQTtoValue ('e' : cs) d v = (v, cs) 
parseQTtoValue ('f' : cs) d v = (v + (1024 `div` (4 ^ d)) , cs)
parseQTtoValue ('p' : cs) d v = 
    let (st1, cs1) = parseQTtoValue cs (d + 1) v
        (st2, cs2) = parseQTtoValue cs1 (d + 1) v
        (st3, cs3) = parseQTtoValue cs2 (d + 1) v
        (st4, cs4) = parseQTtoValue cs3 (d + 1) v
    in (st1 + st2 +  st3 + st4, cs4)


-- Input handler, converts file into data structure
readFiler:: [String] -> [(String, String)] -> [(String, String)]
readFiler [] outputs = outputs
readFiler inputs outputs =
    let tcase = take 2 inputs
        newList = drop 2 inputs
        a = head tcase
        b = last tcase
        tc = (a, b)
    in readFiler newList (outputs ++ [tc])


-- Handler for each test case
printQuad :: [(String, String)] -> [String] -> String
printQuad (x:xs) ys = printQuad xs (ys ++ [printStruct x])
printQuad [x] ys = unlines $ (ys ++ [printStruct x])
printQuad [] ys = unlines $ ys

-- Compute and print results
printStruct :: (String, String) -> String
printStruct (t1, t2) = 
    let res1 = cleanQuery(parseQTtoValue t1 0 0)
        res2 = cleanQuery(parseQTtoValue t2 0 0)
        totalSum = res1 + res2
    in show("test case: " ++  t1 ++ "(" ++ (show res1) ++ ") + " ++ t2 ++ "(" ++ (show res2) ++ ") = " ++ (show(totalSum)))

-- Parser for datastructure
cleanQuery :: (Int, String) -> Int
cleanQuery (val, discard) = val 