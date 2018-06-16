{-# LANGUAGE ParallelListComp #-}

module Fibonacci where

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]


-- exercise 2
fibs2 :: [Integer]
fibs2 = 0:1:[ x + y
            | x <- fibs2
            | y <- tail fibs2]


-- exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a: streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList


-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed g a = Cons a (streamFromSeed g (g a))


-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s1) s2 = Cons a (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = interleaveStreams a (interleaveStreams b (interleaveStreams c d))
  where
    a = streamRepeat 0
    b = streamRepeat 1
    c = streamRepeat 2
    d = streamFromSeed (+1) 3
