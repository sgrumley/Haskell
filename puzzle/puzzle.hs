module Main (main) where 

data Piece = Piece{
    content::[String],
    edges::String
} deriving (Show)

data Puzzle = Puzzle{
    size::Int,
    width::Int,
    totalHeight::Int,
    order::[Piece]
} deriving (Show)



main::IO()
main = interact driver

driver:: String -> String
driver file =
    let strLines = lines file
        nTests = head strLines
        newList = drop 1 strLines 
        -- for each case
        spec = head newList
        tcData = map read $ words spec :: [Int]
        pSize = tcData!!0 
        pieceList = drop 1 newList 
        pieces = take ((pSize + 2)* (2 * pSize)) pieceList
        tc1 = Puzzle {size = pSize, width = tcData!!2, totalHeight = tcData!!1, order = (getPieces (pSize) pieces [])}
        rc1 = solver tc1 (order tc1) ""
    in rc1--show tc1

-- number of lines to pull -> String of file input-> return array of pieces
getPieces:: Int -> [String] -> [Piece] -> [Piece]
getPieces size [] pList = pList 
getPieces size file pList =
    let i = take (size + 1) file
        e = last i
        --e = i !! (length i - 2)
        ip = Piece {content = (take size i), edges = e}
        newList = drop (size + 2) file
    in getPieces size newList (pList ++ [ip])

-- Solve
solver:: Puzzle -> [Piece] -> String -> String
solver puz [] res = res
solver puz pList res = 
    let p1 = head pList
        a = drop 1 pList
        t = (content p1)
        s = unlines t
    in solver puz a (res ++ s)

-- Get test cases ioto array