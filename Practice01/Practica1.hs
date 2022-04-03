module EAB where

--debe de ser como el de las notas
data EAB = Var String --
         | Num Int -- 
         | B Bool --f = B False --t = B True 
         | Sum EAB EAB --
         | Prod EAB EAB --
         | Neg EAB --
         | Pred EAB
         | Suc EAB
         | And EAB EAB -- And f t -> And (B False) (B True)
         | Or EAB EAB
         | Not EAB
         | Iszero EAB 
         | If EAB EAB EAB
         | Let EAB EAB
--         | MatchNat EAB EAB EAB
--         | GT EAB EAB
         | Abs String EAB  --Operador para el ligado de variables
        -- y delegarle la tarea de revisar las cosas especiales
         deriving (Show)

-- semantica dinámica se refiere al valor de un programa (solo importa el resultado)
-- semantica estática se refiere a que este bien tipado y que no tenga FV

--Paso pequeño, todos los casos de la relación de la nota
--evaluar primero el lado izq y luego el lado der
--solo en el let primero necesitas primero llegar al valor para despues sustituir
-- Ejemplo IF
-- eval1 (If (Sum (Num 1) (Num 2)) (Num 3) (Num 4)) 

eval1 :: EAB -> EAB
eval1 (Var x) = Var x
eval1 (B y) = B y
eval1 (Num n) = Num n
eval1 (Sum e1 e2) = case (e1,e2) of
                     (Num n, Num m) -> Num (n+m)
                     (Num n, e) -> Sum (Num n) (eval1 e)
                     (_,_) -> Sum (eval1 e1) e2                     
eval1 (Prod e1 e2) = case (e1,e2) of
                     (Num n, Num m) -> Num (n*m)
                     (Num n, e) -> Prod (Num n) (eval1 e)
                     (_,_) -> Prod (eval1 e1) e2
eval1 (Pred e1) = case e1 of
                    (Num n) -> Num (n-1)
                    _ -> Pred (eval1 e1)
eval1 (Neg e1) = case e1 of
                   (Num n) -> Num(-n)
                   _ -> Neg (eval1 e1)
eval1 (Suc e1) = case e1 of
                   (Num n) -> Num (n+1)
                   _ -> Suc (eval1 e1)                   
eval1 (And e1 e2) = case (e1, e2) of
                      (B b1, B b2) -> B (b1 && b1)
                      (B b, e) -> And (B b) (eval1 e)
                      (_, _) -> And (eval1 e1) e2
eval1 (Or e1 e2) = case (e1, e2) of
                    (B b1, B b2) -> B (b1 || b1)
                    (B b, e) -> Or (B b) (eval1 e)
                    (_, _) -> Or (eval1 e1) e2
eval1 (Not e1) = case e1 of
                  (B b) -> B (not b)
                  _ -> Not (eval1 e1)
eval1 (Iszero e1) = case e1 of
                  (Num n) -> B (n == 0)
                  _ -> Iszero (eval1 e1)
eval1 (If e1 e2 e3) = case e1 of
                        (B (True)) -> e2
                        (B (False)) -> e3
                        e -> If (eval1 e) e2 e3                   
eval1 (Let e1 all@(Abs x e2)) = case e1 of
                      (Num n) -> subs e2 (x, e1)
                      (B b) -> subs e2 (x, e1)
                      (e) -> Let (eval1 e) all
-- a = Let (Prod (Num 25) (Num 2)) (Abs "x" (Sum (Num 30) (Var "x")))
eval1 e = e

type Subst = (String, EAB) -- [x:=e]

subs :: EAB -> Subst -> EAB
subs (Var s) (x, e) = if s == x
                      then e
                      else Var s
subs (Sum e1 e2) s = Sum (subs e1 s) (subs e2 s)
subs (Prod e1 e2) s = Prod (subs e1 s) (subs e2 s)
subs (Neg e) s = Neg (subs e s)
subs (Pred e) s = Pred (subs e s)
subs (Suc e) s = Suc (subs e s)
subs (And e1 e2) s = And (subs e1 s) (subs e2 s)
subs (Or e1 e2) s = Or (subs e1 s) (subs e2 s)
subs (Not e) s = Not (subs e s)
subs (Iszero e) s = Iszero (subs e s)
subs (If e1 e2 e3) s = If (subs e1 s) (subs e2 s) (subs e3 s)
subs (Let e1 e2) s = Let (subs e1 s) (subs e2 s)
subs (Abs z e) s@(x,r)
  | z == x || elem z (fv r) = error "No se puede aplicar la substitución"
  | otherwise = Abs z (subs e s)
subs e s = e

fv :: EAB -> [String]
fv (Var str) = [str]
fv (Sum e1 e2) = fv e1 ++ fv e2
fv (Prod e1 e2) = fv e1 ++ fv e2
fv (Neg e) = fv e
fv (Pred e) = fv e
fv (Suc e) = fv e
fv (And e1 e2) = fv e1 ++ fv e2
fv (Or e1 e2) = fv e1 ++ fv e2
fv (Not e) = fv e
fv (Iszero e) = fv e
fv (If e1 e2 e3) = fv e1 ++ fv e2 ++ fv e3
fv (Let e1 e2) = fv e1 ++ fv e2
fv (Abs x e) = filter (/= x) (fv e)

-- Ejemplos

-- >>> evals $ Sum (Sum (Num 3) (Num 4)) (Sum (Num 5) (Num 6))
-- >>> evals $ Sum (Sum (Num 3) (Num 4)) (Sum (Num 5) (Sum (Num 8) (Num 9)))

-- >>> evals $ Pred $ And (B True) (B False)

-- >>> evals $ If (Sum (Num 4) (Num 6)) (Num 4) (Num 7)
-- >>> evals $ If (Iszero (Num 0)) (Num 5) (Num 7)

-- >>> evals $ Let (Sum (Num 3) (Num 4)) (Abs "x" (Prod (Var "x") (Num 7)))
-- >>> evals $ Let (Sum (Num 3) (B True)) (Abs "x" (Prod (Var "x") (Num 7)))
-- >>> evals $ Let (Sum (Num 3) (Num 4)) (Abs "x" (Prod (Var "x") (B True)))

-- >>>  evals $ Sum (Prod (Num 2) (Num 6)) (B True)
-- >>>  evals $ Sum (Prod (Num 2) (Num 6)) (B True)
-- >>> evals $ Iszero (And (B True) (B False))
-- >>> Sum (Prod (Num 2) (Num 6)) (B True)

-- 
evals :: EAB -> EAB 
evals (Sum e1 e2) = case (evals e1, evals e2) of
                     (Num n, Num m) -> Num (n+m)
                     (e1,e2) -> Sum e1 e2

evals (Prod e1 e2) = case (evals e1, evals e2) of
                     (Num n, Num m) -> Num (n*m)
                     (e1,e2) -> Prod e1 e2

evals (Neg e1) = case evals e1 of
                   (Num n) -> Num(-n)
                   e -> Neg e 

evals (Pred e1) = case evals e1 of
                    (Num n) -> Num (n-1)
                    e -> Pred e
  
evals (Suc e1) = case evals e1 of
                   (Num n) -> Num (n+1)
                   e -> Suc e 

evals (And e1 e2) = case (evals e1, evals e2) of
                      (B b1, B b2) -> B (b1 && b2)
                      (e1, e2) -> And e1 e2

evals (Or e1 e2) = case (evals e1, evals e2) of
                    (B b1, B b2) -> B (b1 || b1)
                    (e1, e2) -> Or e1 e2

evals (Not e1) = case evals e1 of
                  (B b) -> B (not b)
                  e -> Not e

evals (Iszero e1) = case evals e1 of
                  (Num n) -> B (n == 0)
                  (e) -> Iszero e

evals (If e1 e2 e3) = case evals e1 of
                        (B (True)) -> evals $ e2
                        (B (False)) -> evals $ e3
                        e -> If e e2 e3

evals (Let e1 all@(Abs x e2)) = case evals e1 of
                      (Num n) -> evals $ subs e2 (x, (Num n))
                      (B b) -> evals $ subs e2 (x, (B b))
                      e -> Let e all 
evals e = e

-- Ejemplos

-- >>> eval $ Sum (B True) (Num 1)
-- >>> eval $ And (B True) (Num 3)
--- >>> eval $ Sum (Num 3) (Num 8)
--- >>> eval $ Sum (Prod (Num 2) (Num 6)) (Num 3)
eval :: EAB -> EAB
eval e@(Sum e1 e2) = case evals e of
                      Num n -> Num n
                      _ -> error "La suma funciona con numeros"

eval e@(Prod e1 e2) = case evals e of
                     Num n -> Num n
                     _ -> error "El producto funciona con numeros"

eval e@(Neg e1) = case evals e of
                   (Num n) -> Num n
                   _ -> error "Solo funciona con numeros" 

eval e@(Pred e1) = case evals e of
                   (Num n) -> Num n
                   _ -> error "Solo funciona con numeros"
  
eval e@(Suc e1) = case evals e of
                   (Num n) -> Num n
                   _ -> error "Solo funciona con numeros" 

eval e@(And e1 e2) = case evals e of
                      (B b) -> B b
                      _ -> error "No booleano"

eval e@(Or e1 e2) = case evals e of
                    (B b) -> B b
                    _ -> error "No booleano"

eval e@(Not e1) = case evals e of
                  (B b) -> B b
                  _ -> error "No booleano"

eval e@(Iszero e1) = case evals e of
                  (B b) -> B b
                  _ -> error "Unicamente numeros"

eval e@(If e1 e2 e3) = case evals e of
                      (Num n) -> Num n
                      (B b) -> B b
                      _ -> error "Expresion bloqueada" 

eval e@(Let e1 e2) = case evals e of
                      (Num n) -> Num n
                      (B b) -> B b
                      _ -> error "Expresion bloqueada" 
eval e = e   


data Type = TNum | TBool deriving (Show, Eq)

type Ctx = [(String, Type)] 

-- Ejemplos

-- >>> vt [("x", TNum), ("y", TBool)] (Var "y") TNum
-- >>> vt [] Sum (Num 4) (Num 5) TNum
-- >>> vt [] Sum (Num 4) (B False) TNum
-- >>> vt [("x", TNum)] Sum (Num 4) (Var "x") TNum
-- >>> vt [] Let (Sum (Num 1) (Num 2)) (Iszero (Prod (Sum (Num 5) (Var "x")) (Sum (Num 2) (Var "x")))) TBool
-- 

vt :: Ctx -> EAB -> Type -> Bool
vt [] (Var x) t' = False
vt [] (Num n) TNum = True
vt [] (B b) TBool = True

vt ((v, t):xs) e@(Var x) t' | t == t' && x == v = True
                            | otherwise = vt xs e t'

vt [(v, t)] (Num n) t'@TNum = t' == t
vt [(v, t)] (B b) t' = case t' of
                        TBool -> True
                        _ -> False

vt g (Sum e e') t'@TNum = vt g e t' && vt g e' t' 
vt g (Prod e e') t'@TNum = vt g e t' && vt g e' t'
vt g (Pred e) t'@TNum = vt g e t'
vt g (Suc e) t'@TNum = vt g e t'
vt g (If e e' e'') t' = vt g e TBool && vt g e' t' && vt g e'' t'
vt g (Iszero e) TBool = vt g e TNum  

vt g (Let e (Abs x e2)) t' = case (vt g e TNum) of
                              True -> vt ((x, TNum):g) e2 t'
                              _ -> vt ((x, TBool):g) e2 t'


evalt :: EAB -> EAB
evalt _ = error "Implementar"