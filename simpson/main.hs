import Simpson

myFunc :: Double -> Double
myFunc = \x -> x 

main :: IO ()
main = do
  let result = Simpson.integrate myFunc 0.0 8.0 4
  putStrLn $ show result