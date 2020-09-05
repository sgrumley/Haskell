qsort :: Ord a => [a] -> [a]
qsort xs = case xs of
   []     -> []
   x : xs -> qsort [x' | x' <- xs, x' < x]
             ++ [x] ++
             qsort [x' | x' <- xs, x' >= x]
