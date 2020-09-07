module Main (main) where 
import Simpson

myFunc :: Double -> Double
myFunc = \x -> x*x 

main::IO()
main = interact drivers

drivers:: String -> String
drivers file =
  -- pass func, lowerbound, upperbound, n
  let result = Simpson.driver myFunc 2.0 8.0 3
  in (show result) ++ "\n" 