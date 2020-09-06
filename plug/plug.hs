
main::IO()
main = interact driver

driver:: String -> String
driver file =
    let strLines = lines file
        nSockets = read (head strLines) :: Int
        newList = drop 1 strLines 
        sockets = take nSockets newList
        s = drop nSockets newList

        nPlugs = read (head s) :: Int
        newList1 = drop 1 s
        plugsFull = take nPlugs newList1
        plugs = stripPlug plugsFull []
        p = drop nPlugs newList1

        nAdaptors = read (head p) :: Int
        newList2 = drop 1 p
        adaptors = take nAdaptors newList2

        -- match all possible plugs to sockets
        -- try again with adaptors
                
    in unlines plugs

stripPlug :: [String] -> [String] -> [String]
stripPlug [] output = output
stripPlug input output = 
    let h = head input
        nl = drop 1 input
        value = last h
        --keep last char
    in stripPlug nl (output ++ [(show value)])