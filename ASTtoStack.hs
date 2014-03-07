module ASTtoStack where

import StackMachine (Inst)
import qualified Stackmachine as S

import AST (Term)
import qualified AST as A

compile :: Term -> [Inst]
compile (A.Var i) = [S.Push $ Right i]
compile (A.Plus t1 t2) = (compile t1) ++ (compile t2) ++ [S.Add]
compile (A.Subtract t1 t2) = (compile t1) ++ (compile t2) ++ [S.Sub]
compile (A.Number i) = [S.Push $ Left i]
compile (A.If t1 t2 t3) = (compile t1) ++ (compile t2) ++ (compile t3) ++ [S.If]
compile (A.While t1 t2) = [S.While (compile t1) (compile t2)] 
compile (A.Lt t1 t2) = (compile t1) ++ (compile t2) ++ [S.Lt]
compile (A.Not t) = (compile t) ++ [S.Not]
compile (A.Eq t1 t2) = (compile t1) ++ (compile t2) ++ [S.Eq]
compile (A.Or t1 t2) = (compile t1) ++ (compile t2) ++ [S.Or]
compile (A.Set i t) = (compile t) ++ [S.Set i]

-- todo: figure out how exactly to handle while loops