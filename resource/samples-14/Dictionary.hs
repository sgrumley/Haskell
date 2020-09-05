module Dictionary (Dictionary(..)) where

class Dictionary d where
   
   -- minimal complete definition:
   --    empty, isEmpty, update or pUpdate, 
   --    get, toList, delete

   empty :: Ord k => d k v

   isEmpty :: Ord k => d k v -> Bool
   
   update :: Ord k => 
      (v -> v -> v) -> d k v -> k -> v -> d k v
   update f d = curry (pUpdate f d)
   
   pUpdate :: Ord k => 
      (v -> v -> v) -> d k v -> (k,v) -> d k v
   pUpdate f d = uncurry (update f d)
      
   put :: Ord k => d k v -> k -> v -> d k v
   put = update const
   
   pPut :: Ord k => d k v -> (k,v) -> d k v  
   pPut = pUpdate const
    
   get :: Ord k => k -> d k v -> Maybe v

   delete :: Ord k => d k v -> k -> d k v
  
   fromList :: Ord k => [(k,v)] -> d k v
   fromList = foldl pPut empty
   
   toList :: Ord k => d k v -> [(k,v)]
