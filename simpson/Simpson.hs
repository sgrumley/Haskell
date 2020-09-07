
module Simpson(
    integrate,
    points,
    driver
) where
   
-- Name general functions as Func
type Func = Double -> Double

rev :: [Double] -> [Double]
rev [] = []
rev (a:x) = rev x ++ [a]

driver:: Func -> Double -> Double -> Int -> Double 
driver f a b n =
    let h = calcH a b n
        p = points a b h []
       -- pr = rev p
        res = calculateHandler f p n 0 0.0 h

    in res

integrate:: Func -> Double -> Double -> Int -> Double 
integrate f a b n = 
    let h = calcH a b n
    in h * (f a + 4 * f a + f b)/3 


calcH:: Double -> Double -> Int -> Double
calcH a b n = (b - a) / (fromIntegral n)


points  ::  Double -> Double -> Double -> [Double] -> [Double]
points  a b h res
    | b <= a = (res ++ [b])
    | otherwise =
        let i = b - h
        in points a i h (res ++ [b])

-- points  :: Func -> Double -> Double -> Double -> [Double] -> [Double]
-- points f a b h res
--     | b <= a = (res ++ [b])
--     | otherwise =
--         let i = a + h--(fromIntegral n)
--             value = f a
--         in points f i b h (res ++ [value])

calculateHandler :: Func -> [Double] -> Int -> Int -> Double -> Double -> Double
calculateHandler f points n iter res h = ((calculate f points n iter res h) * h) / 3

calculate :: Func -> [Double] -> Int -> Int -> Double -> Double -> Double
calculate f points n iter res h
    | iter == 0 =
        let fx = f (points!!iter) 
        in calculate f points n (iter + 1) (res + fx) h

    | iter >=  n = 
        let fx = f (points!!iter)          
        in res + fx 

    | iter `mod` 2 /= 0 = 
        let fx = f (points!!iter)
            r = 4 * fx
        in calculate f points n (iter + 1) (res + r) h

    | iter `mod` 2 == 0 = 
        let fx = f (points!!iter)
            r = fx * 2--2 * fx
        in calculate f points n (iter + 1) (res + r) h
    
    | otherwise = res 



