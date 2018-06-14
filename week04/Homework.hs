module Homework where

import Data.List

-- exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x - 2) * y) 1 . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f n
         |  even n = n `div` 2
         | otherwise = 3 * n + 1


-- exercise 2
-- Sorry, I don't know how to generate a balanced binary tree use foldr
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node height (foldTree $ take half xs) (xs !! half) (foldTree $ drop (half + 1) xs)
  where len = length xs
        half = len `div` 2
        height = floor (logBase 2 (fromIntegral len)::Double)


-- exercise 3
xor :: [Bool] -> Bool
xor = odd . foldr (\x y -> if x then (y + 1) else y) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a bs = foldr (\b g x -> g (f x b)) id bs a


-- exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ [1..n] \\ sieve
  where table = cartProd [1..n] [1..n]
        sieve = foldr (\(i, j) t -> let v = i + j + 2 * i * j in if v<= n then v:t else t) [] table
