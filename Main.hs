module Main where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data Bottoms = Bottom | Empty
               deriving (Eq, Show)
data Unary = Step | Eval | If | Car | Cdr | IsPair | IsNum | SwapContinuation
               deriving (Eq, Show)
data Binary = Next | Lambda2 | Quot2 | Cons | SetCar | SetCdr | IsEq | Equals | Plus | Minus | Times | Div | GreaterThan
               deriving (Eq, Show)


data L3Prim = PrimBottom Bottoms
            | PrimUnary  Unary
            | PrimBinary Binary
               deriving (Eq, Show)

type Store  = [(L3Term, L3Term)]

infixr 9 :::
data L3Term = C L3Prim
            | N Int
            | L3Term ::: L3Term
               deriving (Eq, Show)

--      pi         (k:ks)  sigma
step :: L3Term -> (L3Term, Store) -> (L3Term, L3Term, Store)
step pi (k, store) =
  let (k' ::: ks) = fromJust $ lookup k store
  in  case k' of
    (C (PrimUnary f)) -> (applyUnary f pi, ks, store)


applyUnary :: Unary -> L3Term -> L3Term
applyUnary IsNum (N _ ) = C (PrimBottom Empty)

car :: L3Term
car = undefined

-- data Value = VInt Int

-- type Name = String

-- data Exp = Var Name
--          | Lam Name Exp
--          | App Exp Exp
--          | Lit Value
--          | Cond Exp Exp Exp

-- type Env = Map Name Exp

-- lookup :: Exp
-- lookup = Lam (Var )

-- eval1 :: Exp -> Env -> Exp
-- eval1 (Var n)     =  Lam $ fromJust (Map.lookup n env)
-- eval1 (Lam n exp) = s -> eval1 exp (Map.insert n eps env)

-- -- eval1 env (App (Lam n body1) exp2) = eval1 (Map.insert n body1 env) exp2

-- -- eval1 env _ = error "type error: Applied non-function to smth""
