
main::IO()
main = interact driver

driver:: String -> String
driver file =
    let strLines = lines file
        nSockets = read (head strLines) :: Int
        newList = drop 1 strLines 
        socketss = take nSockets newList
        sockets = stripPlug socketss []
        s = drop nSockets newList

        nPlugs = read (head s) :: Int
        newList1 = drop 1 s
        plugsFull = take nPlugs newList1
        plugs = stripPlug plugsFull []
        p = drop nPlugs newList1

        nAdaptors = read (head p) :: Int
        newList2 = drop 1 p
        adaptors = take nAdaptors newList2

        -- First iteration
        test = handler sockets plugs [] -- should return a list of sockets that werent used
        testP = printer test
        -- Second Iteration using adaptors
        --adapt = test adaptors
        -- Third iteration multiple adaptors
                
    in unlines testP

-- checkAdaptors :: ([String], [String]) -> [String] -> ([String], [String])
-- cehckAdaptors (s, p) adaptors =
--     let 

printer:: ([String], [String]) -> [String]
printer (a, b) = b

-- Iterate over sockets and check if they match a plug
handler:: [String] -> [String] -> [String] -> ([String],[String])
handler [] plugs returnList = (returnList , plugs) --returnList
handler sockets plugs returnList =
    let s = head sockets
        nl = drop 1 sockets
        pl = length plugs
        match = connectPlug plugs s []
        ppl = length match
    -- if the length of plugs is the same as when it is returned, means no match
    in if pl == ppl then
        handler nl match (returnList ++ [s])
    else
        handler nl match returnList

            
-- Iterate over plugs comparing to the passed in socket
connectPlug  :: [String] -> String -> [String] -> [String]
connectPlug [] s returnList = returnList
connectPlug plugs s returnList =
    let h = head plugs
        nl = drop 1 plugs
    in if h == s then
        connectPlug nl "z" returnList
        else
            connectPlug nl s (returnList ++ [h])



stripPlug :: [String] -> [String] -> [String]
stripPlug [] output = output
stripPlug input output = 
    let h = head input
        nl = drop 1 input
        value = last h
        --keep last char
    in stripPlug nl (output ++ [(show value)])