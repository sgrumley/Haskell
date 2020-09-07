import Simpson

myFunc :: Double -> Double
myFunc = \x -> x*x 

printD:: [Double] -> String -> String
printD [] s = s
printD listD s = 
  let h = head listD
      nl = drop 1 listD
  in printD nl (s ++ " " ++ (show h))

calcH:: Double -> Double -> Int -> Double
calcH a b n = (b - a) / (fromIntegral n)

main :: IO ()
main =
  let result = Simpson.integrate myFunc 1.0 2.0 100
      test = Simpson.points  1.0 10.0 (calcH 1.0 10.0 10) []
      testFull = Simpson.driver myFunc 4.0 10.0 2
      tester = printD test "" 
  in putStrLn $ show testFull
