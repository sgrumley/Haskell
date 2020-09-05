module Summer (
      Summer, toSummer, fromSummer
   ) where

data Summer n a = Summer n a

toSummer :: Num n => n -> Summer n ()
toSummer s = Summer s ()

fromSummer :: Num n => Summer n a -> n
fromSummer (Summer s _) = s

instance Num n => Functor (Summer n) where

   fmap f (Summer n x) = Summer n (f x)

instance Num n => Applicative (Summer n) where

  pure x = Summer 0 x

  (Summer n f) <*> (Summer n' x) = 
     Summer (n + n') (f x)

instance Num n => Monad (Summer n) where

   (Summer s x) >>= f = 
      let Summer s' x' = f x
      in Summer (s + s') x' 
