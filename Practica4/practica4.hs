module Sintax where

import Data.List

type Identifier = String

data Expr
  = V Identifier -- variable
  | I Int
  | B Bool
    -- Unary operators 
  | Succ Expr
  | Pred Expr
  | Not Expr
  | Iszero Expr
    -- Binary operators
  | Add Expr Expr
  | Mul Expr Expr
  | And Expr Expr
  | Or Expr Expr 
  | Lt Expr Expr -- menor que
  | Gt Expr Expr -- mayor que
  | Eq Expr Expr
  
  | If Expr Expr Expr
  | Let Expr Expr -- let (e1, x.e2)
  | Fn Identifier Expr -- funciones anonimas  
  | App Expr Expr -- e1 e2
  deriving (Eq, Show)  


data Frame
  = SuccF
  | PredF 
  | NotF
  | IszeroF
  
  | AddFL Expr | AddFR Expr
  | MulFL Expr | MulFR Expr
  | AndFL Expr | AndFR Expr 
  | OrFL Expr | OrFR Expr
  
  | LtFL Expr | LtFR Expr
  | GtFL Expr | GtFR Expr 
  | EqFL Expr | EqFR Expr

  | IfF Expr Expr
  | LetM Identifier Expr
  | AppFL Expr | AppFR Expr
  | AllocF
  deriving (Eq, Show)

data Stack = Empty
           | S Frame Stack
           deriving Show

data State = E Stack Memory Expr
           | R Stack Memory Expr
           | P Stack Memory Expr
           deriving Show

eval1 :: State -> State


isValue :: Expr -> Bool
isValue (I n) = True
isValue (B b) = True
isValue (Fn x e) = True
isValue _ = False
