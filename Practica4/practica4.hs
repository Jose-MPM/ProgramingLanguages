module Practica04 where

import Data.List

{- | Practica 04
     
     Equipo: Lobitos

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

  | Raise Expr
  | Handle Expr Identifier Expr -- handle(e1, x.e2)
  deriving (Eq)

instance Show Expr where
  show (V idr) = show idr
  show (I n) = show n
  show (B b) = show b

  show (Succ e) = "suc " ++ show e
  show (Pred e) = "pred " ++ show e
  show (Not e) = "not " ++ show e
  show (Iszero e) = "iszero " ++ show e

  show (Add e1 e2) = show e1 ++ " + " ++ show e2
  show (Mul e1 e2) = show e1 ++ " * " ++ show e2
  show (And e1 e2) = show e1 ++ " & " ++ show e2
  show (Or e1 e2) = show e1 ++ " | " ++ show e2

  show (Lt e1 e2) = show e1 ++ " < " ++ show e2
  show (Gt e1 e2) = show e1 ++ " > " ++ show e2
  show (Eq e1 e2) = show e1 ++ " == " ++ show e2
  
  show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Let e1 (Fn idr e2)) = "let " ++ show idr ++ " = " ++ show e1 ++ " in " ++ show e2
  show (Fn idr e) = "λ" ++ idr ++ "." ++ show e
  show (App e1 e2) = "app " ++ show e1 ++ " " ++ show e2

  show (Raise e) = "raise " ++ show e
  show (Handle e1 idr e2) =  "hanlde " ++ show e1
                             ++ " with " ++ show idr ++ " => "
                             ++ show e2

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

  | RaiseF
  | HandleF Identifier Expr
  deriving (Eq)

data Stack = Empty
           | S Frame Stack
           deriving (Eq)
instance Show Stack where
  show (Empty) = "□"
  show (S f s) = show f ++ " ; " ++ show s

data State = E Stack Expr
           | R Stack Expr
           | P Stack Expr
           deriving (Eq)

-- Crea una instancia de la clase Show para los marcos de acuerdo
-- a la sintaxis descrita en las notas del curso. (1 punto)
instance Show Frame where
  show e = case e of
    (SuccF) -> "Succ( _ )"
    (PredF) -> "Pred( _ )"
    (NotF) -> "Not( _ )"
    (IszeroF) -> "Iszero( _ )"

    (AddFL e) -> "Add( _ " ++ ", " ++ (show e) ++ ")"
    (AddFR e) -> "Add(" ++ (show e) ++ ", " ++ " _ )"
    (MulFL e) -> "Mul( _ " ++ ", " ++ (show e) ++ ")"
    (MulFR e) -> "Mul(" ++ (show e) ++ ", " ++ " _ )"
    (AndFL e) -> "And( _ " ++ ", " ++ (show e) ++ ")"
    (AndFR e) -> "And(" ++ (show e) ++ ", " ++ " _ )"
    (OrFL e) -> "Or( _ " ++ ", " ++ (show e) ++ ")"
    (OrFR e) -> "Or(" ++ (show e) ++ ", " ++ " _ )"

    (LtFL e) -> "Lt( _ " ++ ", " ++ (show e) ++ ")"
    (LtFR e) -> "Lt(" ++ (show e) ++ ", " ++ " _ )"
    (GtFL e) -> "Gt( _ " ++ ", " ++ (show e) ++ ")"
    (GtFR e) -> "Gt(" ++ (show e) ++ ", " ++ " _ )"
    (EqFL e) -> "Eq( _ " ++ ", " ++ (show e) ++ ")"
    (EqFR e) -> "Eq(" ++ (show e) ++ ", " ++ " _ )"

    (IfF e1 e2) -> "If( _ , " ++ show e1 ++ " , " ++ show e2 ++ " )"
    (LetF x e) -> "Let(" ++ (show x) ++ ", _, " ++ (show e) ++ ")"
    (AppFL e) -> "App ( - , " ++ show e ++ " )" 
    (AppFR e) -> "App ( " ++ show e ++ " , - )"

    (RaiseF) -> "Raise ( - )"
    (HandleF x e) -> "Handle ( - , " ++ show x ++ "." ++ show e ++ " )"

instance Show State where
  show e = case e of
    E st expr  -> show st ++ " ≻ " ++ show expr
    R st expr -> show st ++ " ≺ " ++ show expr
    P st expr -> show st ++ " ≺≺ " ++ show expr

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
frVars (Raise e) = frVars e
frVars (Handle e1 x e2) = filter (/= x) (frVars e1 ++ frVars e2) 

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
subst (Raise e) s = Raise $ subst e s
subst (Handle e1 x e2) s@(y, z)
  | x == y || elem x (frVars z) = error "Invalid substitution"
  | otherwise = Handle (subst e1 s) x (subst e2 s)



-- | Dado un estado de la pila de control, devuelve
--   la reducción a un paso.

-- Ejemplos
-- >>> eval1 (E Empty (Add (I 2) (I 3)))
-- >>> eval1 (E (S (AddFL (I 3)) Empty) (I 2))
-- >>> let s1 = 
--- (E Empty (App (If (App (Fn "x" (Not (V "x"))) (B False)) (Fn "y" (Add (V "y") (I 3))) (Fn "z" (Mul (V "z") (I 2)))) (I 0)))
--- eval s1
--- >>> eval1 $ P (S (HandleF "x" (V "x")) Empty) (Raise (B False))
--- >>> eval1 $ P (S SuccF (S (HandleF "x" (V "x")) Empty)) (Raise (B False))
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
-- Raise
eval1 (E s (Raise e)) = E (S RaiseF s) e
eval1 (R (S RaiseF s) v) =
  case v of
    (I n) -> P s (Raise v)
    (B b) -> P s (Raise v)
    (Fn x e) -> P s (Raise v)
-- Handle
eval1 (E s (Handle e1 x e2)) = E (S (HandleF x e2) s) e1
eval1 (R (S (HandleF x e2) s) v) =
  case v of
    (I n) -> R s v
    (B b) -> R s v
    (Fn x e) -> R s v
eval1 (P (S (HandleF x e2) s) (Raise v)) = E s (subst e2 (x, v))
eval1 (P (S f s) (Raise v)) = P s (Raise v)
eval1 (R Empty e) = (E Empty e)

-- solo nos interesan los casos "positivos"
-- los casos negativos regresa lo mismo para saber que esta bloqueado
eval1 e = e


isBlocked :: State -> Bool
isBlocked e = (eval1 $ eval1 e) == e

-- Ejemplos
-- >>> evals (E Empty (App (If (App (Fn "x" (Not (V "x"))) (B False)) (Fn "y" (Add (V "y") (I 3))) (Fn "z" (Mul (V "z") (I 2)))) (I 0)))
-- >>> evals (E (S (AddFL (I 3)) Empty) (I 2))
-- >>> evals (E (S (AddFL (I 3)) Empty) (B True))
-- >>> evals $ P (S SuccF (S (HandleF "x" (V "x")) Empty)) (Raise (B False))
evals :: State -> State
evals e = let e' = eval1 e in
            if isBlocked e'
            then
              e'
            else
              evals e'
-- Ejemplos
-- >>> evale $ Add (I 2) (I 4)
-- >>> evale $ Add (I 2) (Add (I 5) (B True))
-- >>> evale $ Add (I 2) (If (Iszero (I 0)) (I 20) (B True))
-- >>> evale $ Add (I 2) (If (Iszero (I 2)) (I 20) (B True))
evale :: Expr -> Expr
evale e =
  let state = evals $ E Empty e
  in case state of
    (R Empty a@(I n)) -> a
    (R Empty a@(B b)) -> a
    (R Empty a@(Fn x ex)) -> a
    _ -> error "Error en tiempo de ejecucion, que problamente es de tipos"
