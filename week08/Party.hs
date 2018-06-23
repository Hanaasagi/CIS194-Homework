{-# LANGUAGE TypeSynonymInstances #-}

module Party where

import Employee
import Data.Tree
import Data.Monoid

-- exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL eList f) = GL (e:eList) (f + empFun e)

instance Semigroup Fun where
  (<>) = mappend

instance Monoid Fun where
  mempty = 0
  mappend = (+)

instance Semigroup GuestList where
  (<>) = mappend

instance Monoid GuestList where
  mempty = GL mempty mempty
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 <> e2) (f1 <> f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max  -- GuestList is instance of Ord typeclass


-- exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rl ts) = f rl $ map (treeFold f) ts


-- exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss) where
  withoutBoss = mconcat (map (uncurry moreFun) results)
  withBoss = glCons boss (mconcat (map snd results))


-- exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


-- exercise 5
glFormat :: GuestList -> String
glFormat (GL e f) = unlines $ ("Total fun: " ++ show f) : map empName e

parse :: String -> String
parse = glFormat . maxFun . read

main :: IO ()
main = parse <$> readFile "company.txt" >>= putStrLn
