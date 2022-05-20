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
          | Eq Expr Expr
          | If Expr Expr Expr
          | Let Expr Expr
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
        (Arrow e1 e2) -> show e1 ++ " → " ++ show e2
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


-- | Obtiene una variable de tipo fresca.

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

findT :: Identifier -> Ctxt -> Maybe Type
findT _ [] = Nothing
findT x ((y, t): ys) = if x == y 
                       then Just t
                       else findT x ys

-- | Dada una expresión infiere su tipo y obtiene 
--   el conjunto de restricciones para la expresión.

-- Ejemplos
-- >>> rest ([], (Add (V "x") (V "x")))
-- >>> rest ([], Fn "x" (V "x"))
-- >>> b = (Let (B True) (Fn ("x") (And (V "x") (Let (I 10) (Fn ("x") (Eq (I 0) (Succ (V "x"))))))))
-- >>> rest([],b)
-- >>> rest([],Add (I 5) (I 9))
rest :: ([Type], Expr) -> ([Type], Ctxt, Type, Constraint)
rest (xs, V x) = (fresh xs:xs, [(x, fresh xs)], fresh xs, [])
rest (xs, I n) = (xs, [], Integer, [])
rest (xs, B b) = (xs, [], Boolean, [])
rest (xs, Fn x e) = let (t2, c, tp, r) = rest (xs, e)
                      in case findT x c of
                        Just tpA -> (t2,  c \\ [(x, tpA)] , Arrow tpA tp, r)
                        Nothing -> let tpA = fresh t2 
                                   in (t2 `union` [tpA], c, Arrow tpA tp, r)
rest (xs, Succ e) = let (t2, c, tp, r) = rest (xs, e)
                        rf = r ++ [(tp, Integer)]
                     in (t2, c, Integer, rf)
rest (xs, Pred e) = let (t2, c, tp, r) = rest (xs, e)
                        rf = r ++ [(tp, Integer)]
                   in (t2, c, Integer, rf)
rest (xs, Iszero e) = let (t2, c, tp, r) = rest (xs, e)
                          rf = r ++ [(tp, Boolean)]
                        in (t2, c, Boolean, rf)
rest (xs, Not e) = let (t2, c, tp, r) = rest (xs, e)        
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
rest (xs, Lt e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                          (t2, c2, tp2, r2) =  rest (t1, e2)
                          rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                          c =  c1 ++ c2
                          rf = r1 ++ r2 ++ rs ++ [(tp1, Boolean), (tp2, Boolean)]
                        in (t2, c, Boolean, rf) 
rest (xs, Gt e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                          (t2, c2, tp2, r2) =  rest (t1, e2)
                          rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                          c =  c1 ++ c2
                          rf = r1 ++ r2 ++ rs ++ [(tp1, Boolean), (tp2, Boolean)]
                        in (t2, c, Boolean, rf)
rest (xs, Eq e1 e2) = let (t1, c1, tp1, r1) = rest (xs, e1)
                          (t2, c2, tp2, r2) =  rest (t1, e2)
                          rs = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                          c =  c1 ++ c2
                          rf = r1 `union` r2 `union` rs `union` [(tp1, Boolean), (tp2, Boolean)]
                        in (t2, c, Boolean, rf)
rest (xs, If e1 e2 e3) = let (t1, c1, tp1, r1) = rest (xs, e1)
                             (t2, c2, tp2, r2) = rest (t1, e2)
                             (t3, c3, tp3, r3) = rest (t2, e3) 
                             s21 = [(s2, s3)|(x, s2) <- c2, (y, s3) <- c1, x == y] -- c2 Ctxt del tipo de la inferencia de tipo de e2 con t1
                             s23 = [(s2, s3)|(x, s2) <- c2, (y, s3) <- c3, x == y] -- mas restricciones
                             s13 = [(s2, s3)|(x, s2) <- c1, (y, s3) <- c3, x == y]                             
                             ri = r1 ++ r2 ++ r3
                             si = s21 ++ s23 ++ s13
                             r = ri `union` si
                             rf = r ++ [(tp2, tp3), (tp1, Boolean)]
                            in (t3, c1 `union` c2 `union` c3, tp1, r)
rest (xs, Let e1 (Fn x e2)) = let (t1, c1, tp1, r1) = rest (xs, e1)
                                  (t2, c2, tp2, r2) = rest (t1, e2)
                                  s = [(s1, s2)|(x, s1) <- c1, (y, s2) <- c2, x == y]
                                  r = r1 `union` r2 `union` s -- union por si afecta(REPETICIONES)usar ++
                                in case findT x c2 of 
                                    Just tx -> let r' = r ++ [(tp1,tx)]
                                                   c = (c1 `union` c2) \\  [(x,tx)]
                                                in (t2, c, tp2, r')
                                    Nothing -> let newX = fresh t2
                                                   tF = t2 ++ [newX]
                                                   rF = r `union` [(tp1,newX)]
                                                  in (tF, c1 `union` c2, tp2, rF)
rest (t, App e1 e2) = let (t1, c1, tp1, r1) = rest (t, e1)
                          (t2, c2, tp2, r2) = rest (t1, e2)
                          s = [(s1, s2) | (x, s1) <- c1, (y, s2) <- c2, x == y]
                          newX = fresh t2
                          t' = t2 `union` [newX]
                          r = r1 ++ r2 ++ s ++ [(tp1 , Arrow tp2 newX)]
                      in (t', c1 `union` c2, newX, r)
-- rest e = error "Falta implementar"

-- | Definimos el tipo sustitución
type Substitution = [(IdentifierT, Type)]

-- | Dado un tipo y una sustitución, regresa la
-- aplicion de la sustitucion al tipo.

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

-- | Realiza la composición de dos sustituciones.

-- Ejemplos
-- >>> comp [(1, (Arrow (T 2) (T 3))), (4, T 5)] [(2, T 6)]
comp :: Substitution -> Substitution -> Substitution
comp s1 s2 = noDup $ ((map (\x -> (fst x, subst (snd x) s2 )) s1) ++ s2)

noDup :: (Eq a) => [(a,b)] -> [(a,b)]
noDup (x:xs) = x : noDup (filter (\y-> (fst y) /= (fst x)) xs)
noDup [] = []


-- | Intenta unificar las restricciones, regresa el
--   unificador más general.

-- Ejemplos de la nota 7
-- Ejemplo 3.1 Sí se unifica
-- >>> unif [(Arrow (T 1) (T 2), Arrow (Arrow (T 3) (T 4)) (T 5))]
-- Ejemplo 3.2 No se unifica
-- >>> unif [(T 1, Arrow (T 2) (T 3)), (T 1, T 2)]
unif :: Constraint -> Substitution
unif [] = []
unif ((t1, t2):xs)
  | t1 == t2 = unif(xs)
  | otherwise = case (t1, t2) of
                  (Integer, Boolean) -> []
                  (Boolean, Integer) -> []
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

-- infer (Add (I 5) (I 9))
-- infer (And (B False) (B True))
-- infer $ Let (B True) (Fn ("x") (And (V "x") (Let (I 10) (Fn ("x") (Eq (I 0) (Succ (V "x"))))))))
infer :: Expr -> (Ctxt, Type)
infer e = let (_, c, tp, r) = rest ([], e)
              mgu = unif r
          in (ctxtS c mgu, subst tp mgu)

-- Función auxiliar para aplicar una sustitución a un contexto.
ctxtS :: Ctxt -> Substitution-> Ctxt
ctxtS [] _ = []
ctxtS ((x, t):xs) l =  (x, subst t l):ctxtS xs l