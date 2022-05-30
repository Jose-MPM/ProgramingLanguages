module MiniC where

import Data.List
import Data.Maybe

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
  show Succ = "Sucesor"
  show Pred = "Predecesor"
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

  | L {get :: Int}-- direccion de memoria 
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
  show (Un op e) = show op ++ "(" ++ show e ++ ")"
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
  show (While e1 e2) = "While (" ++ show e1 ++ ", " ++show e2 ++ ")"
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
newAddress ms 
  | corrupted ms = error "Memory corrupted."
  | otherwise = L $ (minFree . fst . unzip) ms 

minFree :: [Int] -> Int
minFree xs = minFrom 0 (length xs, xs)

minFrom :: Int -> (Int,[Int]) -> Int
minFrom a (n,xs)
 | n == 0     = a
 | m == b-a   = minFrom b (n - m, vs)
 | otherwise  = minFrom a (m, us)
 where (us, vs) = partition (< b) xs
       b = a + 1 + div n 2 
       m = length us

-- | Dada una dirección de memoria, devuelve el valor contenido en la celda con tal
--   dirección.

-- Ejemplos
-- >>> access 1 [] 
-- >>> access 0 [(1, 1 9), (2, I 22)]
-- >>> access 2 [(0, B False), (2, I 90)]
access :: Address -> Memory -> Maybe Value
access _ [] = Nothing
access n m@((a, v):ms) 
  | corrupted m = error "Memory corrupted."
  | otherwise = lookup n m

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
-- >>> frVars $ Assign (L 2) (Bin Add (V "z") (V "y"))
frVars :: Expr -> [Identifier]
frVars (V idf) = [idf]
frVars (Un op e) = frVars e
frVars (Bin op e1 e2) = frVars e1 ++ frVars e2
frVars (If e1 e2 e3) = frVars e1 ++ frVars e2 ++ frVars e3 
frVars (Let e1 e2) = frVars e1 ++ frVars e2
frVars (Fn x e) = filter (/= x) (frVars e)
frVars (App e1 e2) = frVars e1 ++ frVars e2
frVars (L dir) = []
frVars (Alloc e) = frVars e
frVars (Dref e) = frVars e
frVars (Assign e1 e2) = frVars e1 ++ frVars e2
frVars (Void) = []
frVars (Seq e1 e2) = frVars e1 ++ frVars e2
frVars (While e1 e2) = frVars e1 ++ frVars e2
frVars _ = []

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
subst (Un op e) s = Un op (subst e s)
subst (Bin op e1 e2) s = Bin op (subst e1 s) (subst e2 s)
subst (If e1 e2 e3) s = If (subst e1 s) (subst e2 s) (subst e3 s)
subst (Let e1 e2) s = Let (subst e1 s) (subst e2 s)
subst (Fn x e) s@(y, z)
  | x == y || elem x (frVars z) = error "Invalid substitution"
  | otherwise = Fn x (subst e s)

subst (App e1 e2) s = App (subst e1 s) (subst e2 s)
subst (L i) _ = (L i) -- No se puede hacer una substitucion en una memoria 
subst (Alloc e1) s = Alloc (subst e1 s)
subst (Dref e1) s = Dref (subst e1 s)
subst (Assign e1 e2) s = Assign (subst e1 s) (subst e2 s)
subst (Void) _ = Void
subst (Seq e1 e2) s = Seq (subst e1 s) (subst e2 s)
subst (While e1 e2) s = While (subst e1 s) (subst e2 s)

-- | Dada una memoria y una expresión, devuelva
--   la reducción a un paso.

-- Ejemplos
-- >>> eval1 ([(0, B False)], (Bin Add (I 1) (I 2)))
-- >>> eval1 ([(0, B False)], (Let (I 1) (Fn ("x") (Bin Add (V "x") (I 2)))))
-- >>> eval1 ([(0, B False)], (Assign (L 0 ) (B True)))
-- >>> eval1 ([], While (B True) (Bin Add (I 1) (I 1)))
-- >>> eval1 ([(2, B True)], Alloc (I 3))
eval1 :: (Memory, Expr) -> (Memory, Expr)
eval1 e@(xs, (I n)) = e
eval1 e@(xs, (B b)) = e
eval1 (xs, (Un op e)) = 
  case (op, e) of
    (Succ, (I n)) -> (xs, I (n + 1))
    (Pred, (I n)) -> (xs, I (n - 1))
    (IsZero, (I n)) -> (xs, B (n == 0))
    (Not, (B b)) -> (xs, B (not b))
    (_,_) -> let (ms, e') = (eval1 (xs, e)) in (ms, Un op e')

eval1 (xs, Bin op e1 e2) = 
  case (op, e1, e2) of
    (Add, I n, I m) -> (xs, I (m + n))
    (Mul, I n, I m) -> (xs, I (m*n))
    (Lt, I n, I m) -> (xs, B (n < m))
    (Gt, I n, I m) -> (xs, B (n > m))
    (Eq, I n, I m) -> (xs, B (n == m))
    (And, B b1, B b2) -> (xs, B (b1 && b2))
    (Or, B b1, B b2) -> (xs, B (b1 || b2))
    (_, I n, e) -> let (ms, e') = (eval1 (xs, e)) in (ms, Bin op (I n) e')
    (_, B b, e) -> let (ms, e') = (eval1 (xs, e)) in (ms, Bin op (B b) e')
    (_,_,_) -> let (ms, e') = (eval1 (xs, e1)) in (ms, Bin op e' e2)

eval1 (xs, If e1 e2 e3) = 
  case e1 of
    (B (True)) -> (xs, e2)
    (B (False)) -> (xs, e3)
    (e) -> let (ms, e') = (eval1 (xs, e)) in (ms, If e' e2 e3)

eval1 (xs, Let e1 all@(Fn x e2)) = 
  case e1 of
    (I n) -> (xs, subst e2 (x, e1))
    (B b) -> (xs, subst e2 (x, e1))
    (e) -> let (ms, e') = (eval1 (xs, e)) in (ms, Let e' all)

eval1 (xs, App e1 e2) = 
  case (e1, e2) of
    (I n, e2) -> let (ms, e) = (eval1 (xs, e2)) in (ms, App e1 e)
    (B b, e2) -> let (ms, e) = (eval1 (xs, e2)) in (ms, App e1 e)
    (Fn x e, I n) -> (xs, subst e1 (x, e2))
    (Fn x e, B b) -> (xs, subst e1 (x, e2)) 
    (_,_) -> let (ms, e) = (eval1 (xs, e1)) in (ms, App e e2)

eval1 (xs, Alloc e) = 
  case e of 
    (I n) -> let l = newAddress xs in ((get l, e):xs, l)
    (B n) -> let l = newAddress xs in ((get l, e):xs, l)
    (e) -> let (ms, e') = (eval1 (xs, e)) in (ms, Alloc e')

eval1 (xs, Dref e) =
  case e of 
    (L n) -> let v = access n xs in (xs, fromJust v)
    (e) -> let (ms, e') = (eval1 (xs, e)) in (ms, Dref e')

eval1 (xs, Assign e1 e2) =
  case (e1, e2) of
    (L n, I m) -> let ms = fromJust $ update (n, e2) xs in (ms, Void)
    (L n, B b) -> let ms = fromJust $ update (n, e2) xs in (ms, Void)
    (L n, e) -> let (ms, e') = (eval1 (xs, e2)) in (ms, Assign e1 e')
    (_,_) -> let (ms, e') = (eval1 (xs, e1)) in (ms, Assign e' e2)

eval1 (xs, Seq e1 e2) = 
  case (e1, e2) of
    (Void, e) -> (xs, e)
    (_,_) -> let (ms, e') = (eval1 (xs, e1)) in (ms, Seq e' e2)

eval1 (xs, all@(While e1 e2)) = (xs, If e1 (Seq e2 all) Void)
eval1 e'@(xs, e) = e'


-- Oscar

-- evals ([(0, B False)], (Un Succ $ Un Succ (I 2)))
-- evals ([(0, B False)], Bin Add (Un Succ $ Un Succ (I 2)) (Bin Add (I 2) (I 3)))
-- evals ([(0, I 10)] , If (Un IsZero (I 0)) (Bin Add (I 10) (I 10)) (Un Succ (I 20)))
-- evals ([(0, I 10)] , If (Un Succ (Bin Add (I 10) (I 10))) (Bin Add (I 10) (I 10)) (Un Succ (I 20)))
-- evals ([(0, I 10)] , App (Fn "x" (Bin Add (V "x") (V "x"))) (Bin Add (I 10) (I 10)))
evals :: (Memory, Expr) -> (Memory, Expr)
evals (mem, (Un op e)) =
  case (op, snd $ evals (mem, e)) of
    (Succ, (I n)) -> (mem, I (n + 1))
    (Pred, (I n)) -> (mem, I (n - 1))
    (IsZero, (I n)) -> (mem, B (n == 0))
    (Not, (B b)) -> (mem, B (not b))
    (_, e') -> (mem, Un op e')

evals (mem, (Bin op e1 e2)) =
  let e1' = snd $ evals (mem, e1) -- evaluamos las subexpresiones
      e2' = snd $ evals (mem, e2)
  in case (op, e1', e2') of
    (Add, (I n), (I m)) -> (mem, I (n + m))
    (Mul, (I n), (I m)) -> (mem, I (n * m))
    (And, (B b1), (B b2)) -> (mem, B (b1 && b2))
    (Or, (B b1), (B b2)) -> (mem, B (b1 || b2))
    (Lt, I n, I m) -> (mem, B (n < m))
    (Gt, I n, I m) -> (mem, B (n > m))
    (Eq, I n, I m) -> (mem, B (n == m))
    (_, _, _) -> (mem, Bin op e1' e2')

evals (mem, If e1 e2 e3) =
  case snd $ evals (mem, e1) of
    (B (True)) -> evals (mem, e2)
    (B (False)) -> evals (mem, e3)
    e -> (mem, If e e2 e3)
    
evals (mem, Let e1 all@(Fn x e2)) =
  case snd $ evals (mem, e1) of
    (I n) -> (mem, subst e2 (x, I n))
    (B b) -> (mem, subst e2 (x, B b))
    e -> (mem, Let e all) -- Solo avanzamos si es valor.


evals (mem, App e1 e2) =
  case snd $ evals (mem, e1) of
    (Fn x e) -> let parametro = snd $ evals (mem, e2) in
      evals (mem, subst e (x, parametro))
    e -> (mem, App e e2)

evals (mem, Alloc e) =
  let l = newAddress mem
  in case snd $ evals (mem,e) of
    (I n) -> ((get l, (I n)):mem, l)
    (B b) -> ((get l, (B b)):mem, l)
    (Fn x e') -> ((get l, (Fn x e')):mem, l)
    e' -> (mem, Alloc e') 

  
    -- (Succ, (I n)) -> (m, I (n + 1))
    -- (Pred, (I n)) -> (m, I (n - 1))
    -- (IsZero, (I n)) -> (m, B (n == 0))
    -- (Not, (B b)) -> (m, B (not b))
    -- (_, _) -> (m, Un op e')
evals p = p

--   case eval1 ()
--   case (op, e) of
--     (Succ, (I n)) -> (xs, I (n + 1))
--     (Pred, (I n)) -> (xs, I (n - 1))
--     (IsZero, (I n)) -> (xs, B (n == 0))
--     (Not, (B b)) -> (xs, B (not b))
--     (_,_) -> let (ms, e') = (eval1 (xs, e))
--               in (ms, Un op e')

    
-- evals _ = error "implementar"

evale :: Expr -> Expr
evale _ = error "implementar"


