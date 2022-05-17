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
          | Fn Identifier Expr -- funciones anonimas  
          | App Expr Expr -- e1 e2
           deriving (Show, Eq)

-- | Gramatica para los tipos
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


-- | Obtiene una variable de tipo fresca
-- Ejemplos
-- >>> fresh [T 0, T 1, T 2, T 3]
-- >>> fresh [T 0, T 1, T 3, T 4]

fresh :: [Type] -> Type
fresh xs = T $ minFree $ (fresh' xs)

fresh' :: [Type] -> [Int]
fresh' xs = concat $ map (tvars) xs

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

-- | Obtiene el conjunto de restricciones
--   para la expresión
rest :: ([Type], Expr) -> ([Type], Ctxt, Type, Constraint)


-- | Definimos el tipo sustitución
type Substitution = [(IdentifierT, Type)]

-- | Dado un tipo y una sustitución, regresa la
-- aplicion de la sustitucion al tipo
subst :: Type → Substitution -> Type

-- | Realiza la composición de dos sustituciones 
comp :: Substitution -> Substitution -> Substitution

-- | Intenta unificar las restricciones, regresa el
--   unificador más general
unif :: Constraint -> Substitution

-- | Dada una expresión infiere su tipo devolviendo el
--   contexto donde es valido.
infer :: Constraint -> Substitution

