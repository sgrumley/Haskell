
module Simpson(
    integrate
) where
   
-- Name general functions as Func
type Func = Double -> Double

integrate:: Func -> Double -> Double -> Int -> Double 
integrate f a b n = let h = (b - a) / (fromIntegral n)
                    in h * (f a + 4 * f a + f b)/3 

-- iterate points of integrate