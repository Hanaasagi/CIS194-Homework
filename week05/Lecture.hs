{-# LANGUAGE FlexibleInstances #-}

class Listable a where
  toList :: a -> [Integer]

instance Listable Integer where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Integer] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)
instance Listable (Tree Integer) where
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

instance (Listable a, Listable b) => Listable (a, b) where
  toList (x, y) = toList x ++ toList y
