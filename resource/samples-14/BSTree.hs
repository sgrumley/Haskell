module BSTree (
      BSTree, module Dictionary, 
      traverse, printTree, sumTree
   ) where

import Prelude hiding (traverse)

import Dictionary
import Summer

data BSTree k v =
   Empty | Node k v (BSTree k v) (BSTree k v)

instance Dictionary BSTree where

   empty = Empty
   
   isEmpty Empty = True
   isEmpty _     = False
   
   update f Empty k v  
                  = Node k v Empty Empty
   update f (Node k v l r) k' v'
      | k' < k    = Node k v (update f l k' v') r
      | k' == k   = Node k (f v' v) l r
      | otherwise = Node k v l (update f r k' v')
   
   get k Empty = Nothing
   get k (Node k' v l r)
      | k < k'     = get k l
      | k == k'    = Just v
      | otherwise  = get k r

   delete Empty k = Empty
   delete (Node k v l r) k'
      | k' < k    = Node k v (delete l k') r
      | k' == k   = join l r
      | otherwise = Node k v l (delete r k')
   
   toList Empty          = []
   toList (Node k v l r) = case l of
      Empty            -> 
         (k,v) : toList r
      Node k' v' l' r' -> 
         toList (Node k' v' l' (Node k v r' r))

split :: BSTree k v -> (k, v, BSTree k v)
split (Node k v l r) = case l of
   Empty -> (k, v, r)
   _     -> let (k', v', l') = split l
            in (k', v', Node k v l' r)

join :: BSTree k v -> BSTree k v -> BSTree k v
join Empty t = t
join t Empty = t
join t t'    = let (k,v,t'') = split t'
               in Node k v t t''

traverse' :: Monad m => 
   (k -> v -> m ()) -> BSTree k v -> m ()
traverse' f t = case t of
   Empty        ->
      return ()
   Node k v l r -> 
      traverse' f l >> f k v >> traverse' f r

traverse :: Monad m => 
   (k -> v -> m ()) -> BSTree k v -> m ()
traverse f t = case t of
   Empty        -> return ()
   Node k v l r -> do
      traverse f l
      f k v
      traverse f r
      
printTree :: (Show k, Show v) => BSTree k v -> IO ()
printTree = 
   traverse (\k v -> putStrLn (show k ++ ": " ++ show v))

sumTree :: Num v => BSTree k v -> v
sumTree = fromSummer . traverse (\k v -> toSummer v)

