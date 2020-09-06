
main::IO()
main = interact driver

driver:: String -> String
driver file =
    let strLines = lines file
        nTests = head strLines
        newList = drop 1 strLines 
        
    in unlines newList