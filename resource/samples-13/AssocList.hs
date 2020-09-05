module AssocList (AssocList, module Dictionary)
   where

import Dictionary

data AssocList k v = AssocList [(k,v)]
 
instance Dictionary AssocList where

   empty = AssocList []
   
   isEmpty (AssocList kvs) = null kvs
   
   update f (AssocList kvs) k v = 
      AssocList (upd kvs)
      where
      upd []      = [(k,v)]
      upd ((k',v'):kvs)
         | k < k'    = (k,v) : (k',v') : kvs
         | k == k'   = (k, f v v') : kvs
         | otherwise = (k',v') : upd kvs

   get k (AssocList kvs) = lookup k kvs

   delete (AssocList kvs) k = 
      AssocList (del kvs)
      where
      del []      = []
      del ((k',v):kvs)
         | k < k'    = (k',v) : kvs
         | k == k'   = kvs
         | otherwise = (k',v) : del kvs

   toList (AssocList kvs) = kvs
