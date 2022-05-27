module MiniC where

import Data.List

{- | Practica 03
     
     Equipo:

     * Alejandra Ortega Garcia - 420002495
     * Oscar Ramírez Gutiérrez - 419004283 
     * José Manuel Pedro Méndez - 31507312

-}


-- | Definicion de las expresiones del lenguaje MiniC
type Identifier = String

-- data Expr = V Identifier -- variable
--           | I Int
--           | B Bool
--           | Succ Expr
--           | Pred Expr
--           | Add Expr Expr
--           | Mul Expr Expr
--           | Not Expr
--           | Iszero Expr
--           | And Expr Expr
--           | Or Expr Expr 
--           | Lt Expr Expr -- menor que
--           | Gt Expr Expr -- mayor que
--           | Eq Expr Expr
--           | If Expr Expr Expr
--           | Let Expr Expr
--           | Fn Identifier Expr -- funciones anonimas  
--           | App Expr Expr -- e1 e2
--           | L Int -- direccion de memoria 
--           | Alloc Expr -- crear una celda de memoria y obtener su direccion
--           | Dref Expr -- obtener el valor almacenado en una direccion de memoria
--           | Assign Expr Expr -- almacenar un valor en una celda de memoria
--           | Void
--           | Sew Expr Expr
--           | While Expr Expr
--            deriving (Show, Eq)


data Op
  = Succ
  | Pred
  | Not
  | IsZero
  | Add
  | Mul
  | And
  | Or
  | Lt
  | Gt
  | Eq
  deriving (Eq, Enum)

instance Show Op where
  show Add = "+"
  show Mul = "*"
  show And = "&"
  show Or = "|"
  show Lt = "<"
  show Gt = ">"
  show Eq = "=="
  show Succ = "++"
  show Pred = "--"
  show Not = "¬"
  show IsZero = "isZero"
  
data Expr
  = V Identifier
  | I Int
  | B Bool
  | Un Op Expr
  | Bin Op Expr Expr
  
  | If Expr Expr Expr
  | Let Expr Expr
  | Fn Identifier Expr -- funciones anonimas  
  | App Expr Expr -- e1 e2

  | L Int -- direccion de memoria 
  | Alloc Expr -- crear una celda de memoria y obtener su direccion
  | Dref Expr -- obtener el valor almacenado en una direccion de memoria
  | Assign Expr Expr -- almacenar un valor en una celda de memoria
  | Void
  | Seq Expr Expr
  | While Expr Expr
  deriving (Eq)

instance Show Expr where
  show (V idr) = show idr
  show (I n) = show n
  show (B b) = show b
  show (Un op e) = show op ++ show e
  show (Bin op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (Let e1 (Fn idr e2)) = "let " ++ show idr ++ " = " ++ show e1 ++ " in " ++ show e2
  show (Fn idr e) = "λ" ++ idr ++ "." ++ show e
  show (App e1 e2) = show e1 ++ " " ++ show e2
  show (L dir) = "l " ++ show dir
  show (Alloc e) = "ref " ++ show e
  show (Dref e) = "! " ++ show e
  show (Assign e1 e2) = show e1 ++ " := " ++ show e2
  show (Void) = "()"
  show (Seq e1 e2) = show e1 ++ "; " ++ show e2
  show e = error "implementar"

--Let (I 10) (Fn ("x") (Bin Eq (I 0) (Un Succ (V "x"))))

type Address = Int
type Value = Expr
type Cell = (Address, Value)
type Memory = [Cell]

-- | Dada una memoria, genera una nueva dirección de memoria que no esté
--   contenida en esta.

-- Ejemplos
-- >>> newAddress []
-- >>> newAddress [(0, B False), (2, I 9)]
-- >>> newAddress [( 0, I 21), (1, Void), (2, I 12)]
-- >>> newAddress [(0, I 21), (1, Void), (2, I 12), (1, B True)]
newAddress :: Memory -> Expr
newAddress xs 
  | corrupted xs = error "Memory corrupted."
  | otherwise = L $ (minFree . fst . unzip) xs 

minFree :: [Int] -> Int
minFree xs = minFrom 0 (length xs,xs)

minFrom :: Int -> (Int,[Int]) -> Int
minFrom a (n,xs)
 | n == 0     = a
 | m == b-a   = minFrom b (n - m, vs)
 | otherwise  = minFrom a (m, us)
 where (us, vs) = partition (< b) xs
       b = a + 1 + div n 2 
       m = length us

-- access 1 [] 
-- >>> Nothing
-- access 0 [(1, 1 9), (2, I 22)]
-- >>> Nothing
-- access access 2 [(0, B False), (2, I 90)]
-- >>> Just 90
access :: Address -> Memory -> Maybe Value
access _ [] = Nothing
access n m@((a,v):xs) = if nub [ n | (a,b) <- m,n <- [a]] == [ n | (a,b) <- m,n <- [a]] -- Verificamos que no tengamos direcciones repetidas
                        then 
                          if a == n
                            then Just v
                            else access n xs
                        else error "Memory corrupted" 

-- | Dada una celda y una memoria tal vez actualiza la memoria
-- con la nueva celda.

-- Ejemplos
-- >>> update (1, I 4) [(1, I 5), (2, I 7)]
-- >>> update (1, I 6) [(5, I 6), (3, V "x"), (4, I 9)]
-- >>> update (1, V "y") [(1, I 9), (7, I 8)]
-- >>> update (1, V "y") [(1, I 9), (1, I 8)]
update :: Cell -> Memory -> Maybe Memory
update cell@((dir, val)) memory
  | corrupted memory = error "Memory corrupted."
  | otherwise = case lookup dir memory of
                  Nothing -> Nothing
                  Just v -> Just $ cell : filter (\x -> (fst x) /= dir) memory 

-- | Indica si la memoria es corrupta

-- Ejemplos
-- >>> corrupted $ [(1, I 2), (1, I 3)]
-- >>> corrupted $ [(1, I 2), (2, I 3), (3, V "x")]
-- >>> corrupted $ [(0, I 21), (1, Void), (2, I 12), (1, B True)]
corrupted :: Memory -> Bool
corrupted xs = (corrupted' . fst . unzip) xs

corrupted' :: [Int] -> Bool
corrupted' [] = False
corrupted' (x:xs) = elem x xs || corrupted' xs    

-- | Obtine el conjunto de variable de una expresión

-- Oscar 
-- Ejemplos
-- >>> frVars $ Bin Add (I 4) (V "x")
frVars :: Expr -> [Identifier]
frVars (V idf) = [idf]
frVars (Un op e) = frVars e
frVars (Bin op e1 e2) = frVars e1 ++ frVars e2
frVars (If e1 e2 e3) = frVars e1 ++ frVars e2 ++ frVars e3 
frVars (Let e1 e2) = frVars e1 ++ frVars e2
frVars (Fn x e) = filter (/= x) (frVars e)
frVars (App e1 e2) = frVars e1 ++ frVars e2
frVars _ = []

type Substitution = (Identifier, Expr)


-- b= Bin Add (V "x") (I 34)
-- subs b ("x", I 2)
subst :: Expr -> Substitution -> Expr
subst (V idf) (x, e1) = if x == idf
                        then e1
                        else (V idf)
subst (I n) _ = I n
subst (B b) _ = B b
subst (Un op e) s = Un op (subst e s)
subst (Bin op e1 e2) s = Bin op (subst e1 s) (subst e2 s)
subst (If e1 e2 e3) s = If (subst e1 s) (subst e2 s) (subst e3 s)
subst (Let e1 e2) s = Let (subst e1 s) (subst e2 s)
subst (Fn x e) s@(y, z)
  | x == y || elem x (frVars z) = error "Invalid substitution"
  | otherwise = Fn x (subst e s)
subst (App e1 e2) s = App (subst e1 s) (subst e2 s)
subst (L i) _ = (L i) -- No se puede ahcer una substitucion en una memoria 
subst (Alloc e1) s = Alloc (subst e1 s)
subst (Dref e1) s = Dref (subst e1 s)
subst (Assign e1 e2) s = Assign (subst e1 s) (subst e2 s)
subst (Void) _ = Void
subst (Seq e1 e2) s = Seq (subst e1 s) (subst e2 s)
subst (While e1 e2) s = While (subst e1 s) (subst e2 s)

-- | Dada una memoria y una expresión, devuelva
--   la reducción a un paso.

-- Ejemplos
-- >>> eval1 ([(0, B False)], (Add (I 1) (I 2)))
-- >>> eval1 ([(0, B False)], (Let ”x” (I 1) (Add (V ”x”) (I 2))))
-- >>> eval1 ([(0, B False)], Assig (L 0 ) (B True))
-- >>> eval1 ([], While (B True) (Add (I 1) (I 1)))
eval1 :: (Memory, Expr) -> (Memory, Expr)
eval1 _ = error "implementar"

-- Oscar
evals :: (Memory, Expr) -> (Memory, Expr)
evals _ = error "implementar"

evale :: Expr -> Expr
evale _ = error "implementar"


