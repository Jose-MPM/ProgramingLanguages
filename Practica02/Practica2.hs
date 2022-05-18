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
          deriving (Eq)

instance Show Type where
    show e = case e of
        (T i) -> "T" ++ (show i)
        (Arrow e1 e2) -> show e1 ++ "→" ++ show e2
        Integer -> "Int"
        Boolean -> "Bool"

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

-- | Dada una expresión infiere su tipo
-- | Obtiene el conjunto de restricciones
--   para la expresión
-- rest ([], (Add (Var "y") (Var "x")))
rest :: ([Type], Expr) -> ([Type], Ctxt, Type, Constraint)
rest (xs, V x) = (fresh xs:xs, [(x, fresh xs)], fresh xs, [])
rest (xs, I n) = (Integer:xs, [], Integer, [])
rest (xs, B b) = (Boolean:xs, [], Boolean, [])
rest (t, Succ e) = let (t2, c, tp, r) = rest (t, e)
                       rf = r ++ [(tp, Integer)]
                     in  (t2, c, Integer, rf)
rest (t, Pred e) = let (t2, c, tp, r) = rest (t, e)
                       rf = r ++ [(tp, Integer)]
                   in (t2, c, Integer, rf)
rest (t, Not e) = let (t2, c, tp, r) = rest (t, e)        
                      rf = r ++ [(tp, Boolean)]
                  in (t2, c, Boolean, rf)
rest (xs, Add e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                           (t2, c2, tp2, r2) = rest (t1, e2)
                           rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                           c =  c1 ++ c2
                           rf = r1 ++ r2 ++ rs ++ [(tp1, Integer), (tp2, Integer)]
                        in (t2, c, Integer, rf)
rest (xs, Mul e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                           (t2, c2, tp2, r2) = rest (t1, e2)
                           rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                           c =  c1 ++ c2
                           rf = r1 ++ r2 ++ rs ++ [(tp1, Integer), (tp2, Integer)]
                        in (t2, c, Integer, rf)
rest (xs, And e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                           (t2, c2, tp2, r2) =  rest (t1, e2)
                           rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                           c =  c1 ++ c2
                           rf = r1 ++ r2 ++ rs ++ [(tp1, Boolean), (tp2, Boolean)]
                        in (t2, c, Boolean, rf)
rest (xs, Or e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                          (t2, c2, tp2, r2) =  rest (t1, e2)
                          rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                          c =  c1 ++ c2
                          rf = r1 ++ r2 ++ rs ++ [(tp1, Boolean), (tp2, Boolean)]
                        in (t2, c, Boolean, rf)
rest e = error "Falta implementar"

-- | Definimos el tipo sustitución
type Substitution = [(IdentifierT, Type)]

-- | Dado un tipo y una sustitución, regresa la
-- aplicion de la sustitucion al tipo
-- Ejemplos
-- >>> subst (Arrow (T 1) (T 2)) [(2, Arrow (T 2) (T 3))]
-- >>> subst (Arrow (T 1) (Arrow (T 2) (T 1))) [(1, T 2), (2, T 3)]
subst :: Type -> Substitution -> Type
subst t [] = t  
subst (T n) ((i,tipo):xs) = if n == i
                            then subst tipo xs
                            else subst (T n) xs
subst (Arrow t1 t2) xs = Arrow (subst t1 xs) (subst t2 xs)
subst t _ = t

-- | Realiza la composición de dos sustituciones
-- 
comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = noDup $ ((map (\x -> (fst x, subst (snd x) s2 )) s1) ++ s2)

noDup :: (Eq a) => [(a,b)] -> [(a,b)]
noDup (x:xs) = x : noDup (filter (\y-> (fst y) /= (fst x)) xs)
noDup [] = []


-- | Intenta unificar las restricciones, regresa el
--   unificador más general
--Ejemplos de la nota 7
-- Ejemplo 3.1 Sí se unifica
-- >>> unif [(Arrow (T 1) (T 2), Arrow (Arrow (T 3) (T 4)) (T 5))]
-- Ejemplo 3.2 No se unifica
-- >>> unif [(T 1, Arrow (T 2) (T 3)), (T 1, T 2)]
unif :: Constraint -> Substitution
unif [] = []
unif ((t1, t2):xs)
  | t1 == t2 = unif(xs)
  | otherwise = case (t1, t2) of
                  (Integer, Boolean) -> error "Fail"
                  (Boolean, Integer) -> error "Fail"
                  (T idT, tipo) -> if idT `elem` (tvars tipo)
                                then error "Fail, no se puede unificar" -- no vamos a poder sustituir cosas del tipo X = U - Z 
                                else comp unificacion sustitucion
                                     where sustitucion = [(idT, tipo)]
                                           unificacion = unif $ substC xs sustitucion
                                     
                  (tipo, T idT) -> unif $ (T idT, tipo):xs
                  (Arrow t1 t2, Arrow s1 s2) -> unif $ [(t1, s1), (t2, s2)] ++ xs

substC :: Constraint -> Substitution -> Constraint
substC [] _ = []
substC ((t1,t2):cs) s = (subst t1 s, subst t2 s): (substC cs s) 
                                     
                                    
                                    



-- | Dada una expresión infiere su tipo devolviendo el
--   contexto donde es valido.
infer :: Constraint -> Substitution
infer _ = error "implementar"
