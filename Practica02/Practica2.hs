module Sintax where

import Data.List

type Identifier = String
    
-- | Definicion de las expresiones de MinHs
data Expr = V Identifier -- variable
          | I Int
          | B Bool
          | Succ Expr
          | Pred Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Not Expr
          | Iszero Expr
          | And Expr Expr
          | Or Expr Expr 
          | Lt Expr Expr -- menor que
          | Gt Expr Expr -- mayor que
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | Fn Identifier Expr -- fun  f 
          | App Expr Expr -- e1 e2
           deriving (Show, Eq)





-- |Gramatica para los tipos
-- T identifier ---> T
-- Arrow Type Type ---> T -> T

type IdentifierT = Int

data Type = T IdentifierT
          | Integer | Boolean
          | Arrow Type Type
          deriving (Show, Eq)
          

-- | Definicion del tipo Contexto de tipado
-- variable : Tipo
-- x: T

type Ctxt = [(Identifier, Type)]

-- | Definicion del tipo Restriccion
-- Tipo = Tipo
-- T = S

type Constraint = [(Type, Type)]


-- | Regresa las variables de tipo de un tipo. 
-- Ejemplos
-- >>> tvars $ Arrow (T 1) (T 2)
-- >>> tvars $ Arrow (Arrow (T 1) (T 2)) (T 2)
-- >>> tvars $ Arrow Boolean Boolean

tvars :: Type -> [IdentifierT]
tvars (T identifierT) = [identifierT]
tvars (Integer) = []
tvars (Boolean) = []
tvars (Arrow t1 t2) = nub $ (tvars t1) ++ (tvars t2)  
