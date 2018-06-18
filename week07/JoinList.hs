{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Editor
import Buffer


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- exercise 1
(+++) :: Monoid m =>
         JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty


-- exercise 2
-- (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing

indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing

indexJ i (Append m l r)
  | i < 0 || i > (getSize . size $ m) = Nothing
  | i < lsize = indexJ i l
  | otherwise = indexJ (i - lsize) r
    where lsize = getSize . size . tag $ l

-- jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty

dropJ n l@(Single _ _)
  | n > 0 = Empty
  | otherwise = l

dropJ n whole@(Append m l r)
  | n <= 0 = whole
  | n >= wholeSize = Empty
  | n >= sizel = dropJ (n - sizel) r
  | otherwise = (dropJ n l) +++ r
    where wholeSize = getSize . size $ m
          sizel = getSize . size . tag $ l

-- jlToList (takeJ n jl) == take n (jlToList jl)
takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty

takeJ n l@(Single _ _)
  | n <= 0 = Empty
  | otherwise = l

takeJ n whole@(Append m l r)
  | n <= 0 = Empty
  | n >= wholeSize = whole
  | n >= sizel = l +++ (takeJ (n - sizel) r)
  | otherwise = takeJ n l
    where wholeSize = getSize . size $ m
          sizel = getSize . size . tag $ l


-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldl (\jl s -> jl +++ scoreLine' s) Empty . lines
    where scoreLine' s = Single (scoreString s, 1) s
  line = indexJ
  replaceLine i s jl = takeJ i jl +++ fromString s +++ dropJ (i + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag


main :: IO ()
main =  do
  txt <- readFile "carol.txt"
  runEditor editor $ (fromString txt :: JoinList (Score, Size) String)
