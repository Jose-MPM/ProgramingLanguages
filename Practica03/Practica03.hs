module MiniC where

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

newAddress :: Memory -> Expr
newAddress _ = error "implementar"

access :: Address -> Memory -> Maybe Value
access _ = error "implementar"

-- Oscar
update :: Cell -> Memory -> Maybe Memory
update cell memory = error "implementar"

-- | Indica si la memoria es corrupta

-- Ejemplos
-- >>> corrupted $ [(1, I 2), (1, I 3)]
-- >>> corrupted $ [(1, I 2), (2, I 3), (3, V "x")]
corrupted :: Memory -> Bool
corrupted ((dir, value):xs) = any (\x -> (fst x) == dir) xs


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

subst :: Expr -> Substitution -> Expr
subst _ _ = error "implementar"


eval1 :: (Memory, Expr) -> (Memory, Expr)
eval1 _ = error "implementar"

-- Oscar
evals :: (Memory, Expr) -> (Memory, Expr)
evals _ = error "implementar"

evale :: Expr -> Expr
evale _ = error "implementar"


