module AVLTree (AVLTree, module Dictionary) where

import Dictionary

data AVLTree k v =
    Empty | 
    Node k v (AVLTree k v) (AVLTree k v) Slope

type Slope = Int

instance Dictionary AVLTree where

   empty = Empty
   
   isEmpty Empty = True
   isEmpty _     = False
   
   update f t k v = fst $ upd t
      where
      upd Empty
            = (Node k v Empty Empty 0, 1)
      upd (Node k' v' l r s)
         | k < k'  
            = let (l', c') = upd l
                  c = if s >= 0 && c' == 1 
                         then 1 else 0
              in balance
                 (Node k' v' l' r (s + c'), c) 
         | k == k' 
            = (Node k (f v v') l r s, 0)
         | otherwise
            = let (r', c') =  upd r
                  c = if s <= 0 && c' == 1 
                         then 1 else 0
              in balance 
                    (Node k' v' l r' (s - c'), c)

   get k Empty = Nothing
   get k (Node k' v l r _)
      | k < k'     = get k l
      | k == k'    = Just v
      | otherwise  = get k r

   delete t k = fst $ del t
      where
      del Empty
            = (Empty, 0)
      del (Node k' v l r s)
         | k < k'  
            = let (l', c') = del l
                  c = if s == 1 && c' == -1
                         then -1 else 0
              in balance
                    (Node k' v l' r (s + c'), c) 
         | k == k' 
            = join l r s
         | otherwise
            = let (r', c') =  del r
                  c = if s == -1 && c' == -1 
                         then -1 else 0
              in balance
                    (Node k' v l r' (s - c'), c)
   
   toList Empty            = []
   toList (Node k v l r s) = case l of
      Empty               -> 
         (k,v) : toList r
      Node k' v' l' r' s' -> 
         toList (Node k' v' l'
            (Node k v r' r s') s)

slope :: AVLTree k v -> Slope
slope (Node _ _ _ _ s) = s

balance :: 
   (AVLTree k v, Int) -> (AVLTree k v, Int)
balance (Node k v l r s, c)
   | 1 < s
      = (shiftRight (Node k v l r s), c - 1)
   | s < -1
      = (shiftLeft (Node k v l r s), c - 1)
   | otherwise
      = (Node k v l r s, c)
        where
        shiftRight (Node k v l r s)
           | slope l == -1
              = rotateRight
                   (Node k v (rotateLeft l) r s)
           | otherwise
              = rotateRight (Node k v l r s)
        shiftLeft (Node k v l r s)
           | slope r == 1
              = rotateLeft 
                   (Node k v l (rotateRight r) s)
           | otherwise
              = rotateLeft (Node k v l r s)
        rotateRight
           (Node k v (Node k' v' l' r' s') r s)
           = let (ss, ss') 
                    = case (s, s') of
                         ( 2,  0) -> (-1,  1)
                         ( 2,  1) -> ( 0,  0)
                         ( 2,  2) -> ( 0, -1)
                         ( 1,  1) -> (-1, -1)
                         ( 1,  0) -> (-1,  0)
                         ( 1, -1) -> (-2,  0)
             in Node k' v' l' (Node k v r' r ss')
                   ss
        rotateLeft
           (Node k v l (Node k' v' l' r' s') s)
           = let (ss, ss')
                    = case (s, s') of
                         (-2,  0) -> ( 1, -1)
                         (-2, -1) -> ( 0,  0)
                         (-2, -2) -> ( 0,  1)
                         (-1, -1) -> ( 1,  1)
                         (-1,  0) -> ( 1,  0)
                         (-1,  1) -> ( 2,  0)
             in Node k' v' (Node k v l l' ss') r'
                   ss

split ::
   AVLTree k v -> ((AVLTree k v, Slope), k, v)
split (Node k v l Empty s)
   = ((l, -1), k, v)
split (Node k v l r s)
   = let ((r', c'), k', v') = split r
         c = if s == -1 && c' == -1
                then -1 else 0
     in (balance (Node k v l r' (s - c'), c),
         k', v')

join :: AVLTree k v -> AVLTree k v -> Slope ->
   (AVLTree k v, Slope)
join Empty r _
   = (r, -1)
join l r s
   = let ((l', c'), k', v') = split l
         s' = s + c'
         c = if s == 1 && c' == -1
                then -1 else 0
     in balance (Node k' v' l' r s', c)
