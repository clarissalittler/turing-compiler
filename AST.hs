module AST where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

data Term = Var Int
           | Plus Term Term
           | Subtract Term Term
           | Number Int
           | If Term Term Term
           | While Term Term
           | Lt Term Term
           | Not Term
           | Eq Term Term
           | And Term Term
           | Or Term Term
           | Set Int Term
           deriving (Eq,Show)
             
{- The semantics here is that we're using integers to represent both numbers and booleans, with the idea that booleans are going to be True if > 0 and False if <= 0. So let's write a simple interpreter for this language so that we can pin down the semantics in a more concrete way and have something to test our compiler against. We're picking a general evaluation order where we go left-first and have shortcircuiting "and" and "or".

   In this language, we'll assume that we always have 16 bit integers and when we overflow we won't "go negative" we'll just only keep the proper 16 bits. We'll do this just by using mod. 
-}

eval :: Term -> State (Map Int Int) Int
eval (Plus t1 t2) = do
  n1 <- eval t1
  n2 <- eval t2
  return $ (n1 + n2) `mod` (2^16) 
eval (Subtract t1 t2) = do
  n1 <- eval t1
  n2 <- eval t2
  return $ (n1 - n2) `mod` (2^16)
eval (Number t) = return $ t `mod` (2^16)
eval (If tb tt tf) = do
  b <- eval tb
  if b > 0 then eval tt else eval tf
eval (And t1 t2) = do
  b <- eval t1
  if b > 0 then eval t2 else return 0
eval (Or t1 t2) = do
  b <- eval t1
  if b > 0 then return 1 else eval t2
eval (Not t) = do
  n <- eval t
  if n > 0 then return 0 else return 1
eval (While t1 t2) = do
  n1 <- eval t1 
  if n1 > 0 then eval t2 >> eval (While t1 t2) else return 1 -- we don't distinguish statements and expressions so While statements return a value technically
eval (Var i) = do
  m <- get
  case M.lookup i m of
    Nothing -> error "variable doesn't exist"
    Just v -> return v
eval (Set i t) = do
  n <- eval t
  m <- get
  put (M.updateWithKey (\ k v -> if k == i then Just n else Just v) i m)
  return n
  
topEval :: Term -> Int
topEval t = evalState (eval t) M.empty 