module Sintax where

type Identifier = String
    

{--
 -- Sintaxis Practica1
 -- Para su Practica 2 deben modificar esto (es lo unico que necesitan de su P1)
 -- Agregar y eliminar las cosas que sean necesarias segun la descripcion que se
 -- dio en la especificacionde la practica.
 --}
data Expr = V Identifier
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
          | Lt Expr Expr
          | Gt Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | Fn Identifier Expr -- fun  f 
          | App Expr Expr -- e1 e2
           deriving (Show, Eq)



{-

Gramatica para los tipos

T identifier ---> T
Arrow Type Type ---> T -> T

-}
type IdentifierT = Int

data Type = T IdentifierT
          | Integer | Boolean
          | Arrow Type Type
          
{-
Contexto de tipado
variable : Tipo
x: T
-} 

type Ctxt = [(Identifier, Type)]

{-
Restriccion
Tipo = Tipo
T = S
-} 
type Constraint = [(Type, Type)]
