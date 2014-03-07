module StackMachine where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Data.Either

{- In this file we're going to introduce the linear stack machine and its semantics that we're going to use as the intermediate language for our compiler to a Turing machine. Why? Because linear representations and a stack are easier to emulate with a TM than an actual register machine. -}

type Var = Int
type StackM = State ([Either Int Var],Map Var Int)

data Inst = Push (Either Int Var)
            | Add
            | Sub
            | Lt
            | Eq
            | And
            | Or
            | Pop
            | Set Var
            | Not
            | If
            | While [Inst] {- Test -}  [Inst] {- Body -}
{- how do I do While loops in a stack machine? -}

pop :: StackM Int 
pop = do
  (stack, vs)  <- get
  put $ (tail stack, vs)
  case (head stack) of
    Left i -> return i
    Right v -> return $ fromJust $ M.lookup v vs

push :: Either Int Var -> StackM ()
push v = modify (\ (s,vars) -> (v : s,vars))

update :: Var -> Int -> StackM ()
update v i = modify (\ (s,vars) -> (s, M.insert v i vars))

toBool :: Int -> Bool
toBool i = i > 0

toInt :: Bool -> Int
toInt b = if b then 1 else 0

evalInst :: Inst -> StackM () 
evalInst Add = do
  x <- pop
  y <- pop
  push $ Left $ x + y 
evalInst Eq = do
  x <- pop
  y <- pop
  push $ Left $ toInt $ (toBool x) == (toBool y)
evalInst And = do
  x <- pop
  y <- pop
  push $ Left $ toInt $ (toBool x) && (toBool y)
evalInst Or = do
  x <- pop
  y <- pop
  push $ Left $ toInt $ (toBool x) || (toBool y)
evalInst If = do
  b <- pop
  t <- pop 
  e <- pop
  if (toBool b) then push (Left t) else push (Left e) 
evalInst Lt = do
  x <- pop
  y <- pop
  push $ Left $ toInt $ x < y
evalInst Not = do
  b <- pop
  push $ Left $ toInt $ not (toBool b)
evalInst (Set v) = do
  x <- pop
  update x v
evalInst (While testInsts bodyInsts) = execLoop testInsts bodyInsts
evalInst Pop = pop >> return () {- we might not actually want this in here since it's not "accesible" per se from the language, but it also doesn't really matter -}
evalInst (Push v) = push v
                    
execLoop :: [Inst] -> [Inst] -> StackM ()
execLoop testInsts bodyInsts = do
  mapM_ evalInst testInsts
  v <- pop
  if (toBool v) 
    then mapM_ evalInst bodyInsts >> execLoop testInsts bodyInsts
    else return ()
         
{- holy code duplication batman 
   Also, we want pop to return the integer value that corresponds to the variable. 

  Loops are complicated: I'm thinking that loops are actually going to need to be done as chickens^W, essentially, a "zipper" structure that will keep around the context of other code and then we can actually hold onto those future instructions as a "loop stack" essentially until the first element of the loop works

    MEH actually I'm thinking that loops will just be done by having a separate set of instructions held as part of the loop. It breaks the "linearity" of it, but that's kinda what loops do --- they're not a linear execution, so I think it's okay that it's handled differently.

   -}


topEval :: [Inst] -> Int
topEval ts = last $ lefts $ fst $ (execState (mapM_ evalInst ts) ([],M.empty))
