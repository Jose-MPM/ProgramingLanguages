module Sintax where

import Data.List

{- | Practica 04
     
     Equipo:

     * Alejandra Ortega Garcia - 420002495
     * Oscar Ramírez Gutiérrez - 419004283 
     * José Manuel Pedro Méndez - 31507312

-}

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
  | LetF Identifier Expr
  | AppFL Expr | AppFR Expr
  deriving (Eq, Show)

data Stack = Empty
           | S Frame Stack
           deriving Show

data State = E Stack Expr
           | R Stack Expr
           | P Stack Expr
           deriving Show


-- | Obtine el conjunto de variable de una expresión

-- Ejemplos
-- >>> frVars $ Bin Add (I 4) (V "x")eval1 ([], While (B True) (Bin Add (I 1) (I 1)))
-- >>> frVars $ Assign (L 2) (Bin Add (V "z") (V "y"))
frVars :: Expr -> [Identifier]
frVars (V idf) = [idf]
frVars (Succ e) = frVars e
frVars (Pred e) = frVars e
frVars (Not e) = frVars e
frVars (Iszero e) = frVars e
frVars (Add e1 e2) = frVars e1 ++ frVars e2
frVars (Mul e1 e2) = frVars e1 ++ frVars e2
frVars (And e1 e2) = frVars e1 ++ frVars e2
frVars (Or e1 e2) = frVars e1 ++ frVars e2
frVars (Lt e1 e2) = frVars e1 ++ frVars e2
frVars (Gt e1 e2) = frVars e1 ++ frVars e2
frVars (Eq e1 e2) = frVars e1 ++ frVars e2
frVars (If e1 e2 e3) = frVars e1 ++ frVars e2 ++ frVars e3 
frVars (Let e1 e2) = frVars e1 ++ frVars e2
frVars (Fn x e) = filter (/= x) (frVars e)
frVars (App e1 e2) = frVars e1 ++ frVars e2


type Substitution = (Identifier, Expr)


-- | Dada un expresion y una sustitución, regresa la
--   expresion con la sustitucion.

-- Ejemplos
-- >>> subst (Add (V ”x”) (I 5)) (”x”,I 10)
-- >>> subst (Let ”x” (I 1) (V ”x”)) (”y”, Add (V ”x”) (I 5))
subst :: Expr -> Substitution -> Expr
subst (V idf) (x, e1) 
  | x == idf = e1
  | otherwise = (V idf)

subst (I n) _ = I n
subst (B b) _ = B b
subst (Pred e) s = Pred (subst e s)
subst (Succ e) s = Succ (subst e s)
subst (Not e) s = Not (subst e s)
subst (Iszero e) s = Iszero (subst e s)
subst (Add e1 e2) s = Add (subst e1 s) (subst e2 s)
subst (Mul e1 e2) s = Mul (subst e1 s) (subst e2 s)
subst (And e1 e2) s = And (subst e1 s) (subst e2 s)
subst (Or e1 e2) s = Or (subst e1 s) (subst e2 s)
subst (Lt e1 e2) s = Lt (subst e1 s) (subst e2 s)
subst (Gt e1 e2) s = Gt (subst e1 s) (subst e2 s)
subst (Eq e1 e2) s = Eq (subst e1 s) (subst e2 s)
subst (If e1 e2 e3) s = If (subst e1 s) (subst e2 s) (subst e3 s)
subst (Let e1 e2) s = Let (subst e1 s) (subst e2 s)
subst (Fn x e) s@(y, z)
  | x == y || elem x (frVars z) = error "Invalid substitution"
  | otherwise = Fn x (subst e s)

subst (App e1 e2) s = App (subst e1 s) (subst e2 s)


-- | Dado un estado de la pila de control, devuelve
--   la reducción a un paso.

-- Ejemplos
-- >>> eval1 (E Empty (Add (I 2) (I 3)))
-- >>> eval1 (E (S (AddFL (I 3)) Empty) (I 2))
-- >>> let s1 = 
--- (E Empty (App (If (App (Fn "x" (Not (V "x"))) (B False)) (Fn "y" (Add (V "y") (I 3))) (Fn "z" (Mul (V "z") (I 2)))) (I 0)))
--- eval s1

eval1 :: State -> State
eval1 (E s e@(I n)) = R s e
eval1 (E s e@(B b)) = R s e
eval1 (E s e@(Fn x exp)) = R s e 
-- Suc
eval1 (E s (Succ e)) = E (S SuccF s) e
eval1 (R (S SuccF s) (I v)) = R s (I (v + 1))
-- Pred 
eval1 (E s (Pred e)) = E (S PredF s) e
eval1 (R (S PredF s) (I v)) = R s (I (v + 1))
-- Not
eval1 (E s (Not e)) = E (S NotF s) e 
eval1 (R (S NotF s) (B v)) = R s (B (not v))
-- Iszero
eval1 (E s (Iszero e)) = E (S IszeroF s) e
eval1 (R (S IszeroF s) (I v)) = R s (B (v == 0))
-- Add 
eval1 (E s (Add e1 e2)) = E (S (AddFL e2) s) e1
eval1 (R (S (AddFL e2) s) v) = E (S (AddFR v) s) e2
eval1 (R (S (AddFR (I v1)) s) (I v2)) = R s (I (v1 + v2))
-- Mul
eval1 (E s (Mul e1 e2)) = E (S (MulFL e2) s) e1
eval1 (R (S (MulFL e2) s) v) = E (S (MulFR v) s) e2
eval1 (R (S (MulFR (I v1)) s) (I v2)) = R s (I (v1 * v2))
-- And
eval1 (E s (And e1 e2)) = E (S (AndFL e2) s) e1
eval1 (R (S (AndFL e2) s) v) = E (S (AndFR v) s) e2
eval1 (R (S (AndFR (B v1)) s) (B v2)) = R s (B (v1 && v2))
-- Or
eval1 (E s (Or e1 e2)) = E (S (OrFL e2) s) e1
eval1 (R (S (OrFL e2) s) v) = E (S (OrFR v) s) e2
eval1 (R (S (OrFR (B v1)) s) (B v2)) = R s (B (v1 || v2))
-- Lt
eval1 (E s (Lt e1 e2)) = E (S (LtFL e2) s) e1
eval1 (R (S (LtFL e2) s) v) = E (S (LtFR v) s) e2
eval1 (R (S (LtFR (I v1)) s) (I v2)) = R s (B (v1 < v2))
-- Gt
eval1 (E s (Gt e1 e2)) = E (S (GtFL e2) s) e1
eval1 (R (S (GtFL e2) s) v) = E (S (GtFR v) s) e2
eval1 (R (S (GtFR (I v1)) s) (I v2)) = R s (B (v1 > v2))
-- Eq
eval1 (E s (Eq e1 e2)) = E (S (EqFL e2) s) e1
eval1 (R (S (EqFL e2) s) v) = E (S (EqFR v) s) e2
eval1 (R (S (EqFR (I v1)) s) (I v2)) = R s (B (v1 == v2))
-- If
eval1 (E s (If e1 e2 e3)) = E (S (IfF e2 e3) s) e1
eval1 (R (S (IfF e2 e3) s) v) = 
  case v of
    (B True) -> E s e2
    (B False) -> E s e3
    _ -> E (S (IfF e2 e3) s) v --- ??
-- Let
eval1 (E s (Let e1 all@(Fn x e2))) =  E (S (LetF x e2) s) e1
eval1 (R (S (LetF x e2) s) v) =
  case v of 
     (I n) -> R s (subst e2 (x, v))
     (B b) -> R s (subst e2 (x, v))
     (Fn x e) -> R s (subst e (x, v))
-- App
eval1 (E s (App e1 e2)) = E (S (AddFL e2) s) e1
eval1 (R (S (AppFL e2) s) v) =  E (S (AppFR v) s) e2
eval1 (R (S (AddFR (Fn x e)) s) v) =
  case v of 
     (I n) -> R s (subst e (x, v))
     (B b) -> R s (subst e (x, v))
     (Fn x e) -> R s (subst e (x, v))

eval1 (R Empty e) = (E Empty e)


isValue :: Expr -> Bool
isValue (I n) = True
isValue (B b) = True
isValue (Fn x e) = True
isValue _ = False
