{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M
import Control.Applicative (liftA2)

-- exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add l r) = eval l + eval r
eval (ExprT.Mul l r) = eval l * eval r


-- exercise 2
evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parseExp ExprT.Lit ExprT.Add ExprT.Mul


-- exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul


-- exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)
{-mul (add (lit 2) (lit 3)) (lit 4) :: Integer-}

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)
{-mul (add (lit 2) (lit 3)) (lit 4) :: Bool-}

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y)= MinMax (max x y)
  mul (MinMax x) (MinMax y)= MinMax (min x y)
{-mul (add (lit 2) (lit 3)) (lit 4) :: MinMax-}

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y
{-mul (add (lit 2) (lit 3)) (lit 4) :: Mod7-}


-- exercise 5
instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


-- exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add = liftA2 $ liftA2 (+)
  mul = liftA2 $ liftA2 (*)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs
{-withVars [("x", 6)] $ add (lit 3) (var "x")-}
