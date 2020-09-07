
module Simpson(
    points,
    driver
) where
   
type Func = Double -> Double

driver:: Func -> Double -> Double -> Int -> Double 
driver f a b n =
    let h = calcH a b n
        p = points f a b h []
        res = calculateHandler f p n 0 0.0 h
    in res

-- Calculate step value
calcH:: Double -> Double -> Int -> Double
calcH a b n = (b - a) / (fromIntegral n)

-- Get the f(x) of all step values and add to list
points  :: Func -> Double -> Double -> Double -> [Double] -> [Double]
points f a b h res
    | b <= a = (res ++ [(f a)])
    | otherwise =
        let i = a + h
            value = f a
        in points f i b h (res ++ [value])

-- final handling after everything has been summed
calculateHandler :: Func -> [Double] -> Int -> Int -> Double -> Double -> Double
calculateHandler f points n iter res h = ((calculate f points n iter res h) * h) / 3

-- for each odd value times f(x) by 4 else * 2
calculate :: Func -> [Double] -> Int -> Int -> Double -> Double -> Double
calculate f points n iter res h
    | iter == 0 =
        let fx = points!!iter
        in calculate f points n (iter + 1) (res + fx) h

    | iter >=  n = 
        let fx = points!!iter          
        in res + fx 

    | iter `mod` 2 /= 0 = 
        let fx = points!!iter
            r = 4 * fx
        in calculate f points n (iter + 1) (res + r) h

    | iter `mod` 2 == 0 = 
        let fx = points!!iter
            r = fx * 2
        in calculate f points n (iter + 1) (res + r) h
    
    | otherwise = res 



