module EAB where
{- | Practica 01
     
     Equipo:

     * Alejandra Ortega Garcia - 420002495
     * Oscar Ramírez Gutiérrez - 419004283 
     * José Manuel Pedro Méndez - 31507312

-}
data EAB = Var String 
         | Num Int 
         | B Bool 
         | Sum EAB EAB 
         | Prod EAB EAB 
         | Neg EAB 
         | Pred EAB
         | Suc EAB
         | And EAB EAB 
         | Or EAB EAB
         | Not EAB
         | Iszero EAB 
         | If EAB EAB EAB
         | Let EAB EAB
         | MatchNat EAB EAB EAB
         | GT EAB EAB
         | Abs String EAB  
         deriving (Show)

-- Ejemplo NEG
-- >>> eval1 (Neg (And (B True) (B False)))

-- Ejemplo IF
-- >>> eval1 (If (Sum (Num 1) (Num 2)) (Num 3) (Num 4))

-- Ejemplo LET
-- >>> eval1 Let (Sum (Var "x") (Num 3)) (Abs ("x") (Sum (Var "x") (Num 5)))
--  

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
                    (B b1, B b2) -> B (b1 || b2)
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
-- >>> evals $ Sum (Sum (Num 3) (Num 4)) (Sum (Num 5) (Num 6))
-- >>> evals $ Sum (Sum (Num 3) (Num 4)) (Sum (Num 5) (Sum (Num 8) (Num 9)))

-- >>> evals $ Pred $ And (B True) (B False)

-- >>> evals $ If (Sum (Num 4) (Num 6)) (Num 4) (Num 7)
-- >>> evals $ If (Iszero (Num 0)) (Num 5) (Num 7)

-- >>> evals $ Let (Sum (Num 3) (Num 4)) (Abs "x" (Prod (Var "x") (Num 7)))
-- >>> evals $ Let (Sum (Num 3) (B True)) (Abs "x" (Prod (Var "x") (Num 7)))
-- >>> evals $ Let (Sum (Num 3) (Num 4)) (Abs "x" (Prod (Var "x") (B True)))
--
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
-- >>> vt [] (Sum (Num 4) (Num 5)) TNum
-- >>> vt [] (Sum (Num 4) (B False)) TNum
-- >>> vt [("x", TNum)] (Sum (Num 4) (Var "x")) TNum
-- >>> vt [] (If (B False) (Num 4) (Num 5)) (TNum)
-- >>> vt [] (Let (Num 1) (Abs ("x") (Sum (Var "x") (Num 1)))) (TNum)
--
vt :: Ctx -> EAB -> Type -> Bool
vt [] (Var x) t' = False
vt _ (Num n) t = case t of 
                  TNum -> True
                  _ -> False

vt _ (B b) t = case t of 
                  TBool -> True
                  _ -> False

vt ((v, t):xs) e@(Var x) t' | t == t' && x == v = True
                            | otherwise = vt xs e t'

vt g (Sum e1 e2) t = case t of 
                      TNum -> vt g e1 t && vt g e2 t
                      _ -> False

vt g (Prod e1 e2) t = case t of 
                      TNum -> vt g e1 t && vt g e2 t
                      _ -> False

vt g (Neg e) t = case t of 
                  TNum -> vt g e t
                  _ -> False

vt g (Pred e) t = case t of 
                  TNum -> vt g e t
                  _ -> False    

vt g (Suc e) t = case t of 
                  TNum -> vt g e t
                  _ -> False 

vt g (And e1 e2) t = case t of 
                      TBool -> vt g e1 t && vt g e2 t
                      _ -> False

vt g (Or e1 e2) t = case t of 
                      TBool -> vt g e1 t && vt g e2 t
                      _ -> False

vt g (Not e) t = case t of 
                  TBool -> vt g e t
                  _ -> False 

vt g (Iszero e) t = case t of 
                  TBool -> vt g e TNum
                  _ -> False

vt g (If e1 e2 e3) t = case (vt g e1 TBool) of 
                        True -> vt g e2 t && vt g e3 t
                        _ -> False


 
vt g (Let e (Abs x e2)) t' = case (vt g e TNum) of
                              True -> vt ((x, TNum):g) e2 t'
                              _ -> vt ((x, TBool):g) e2 t'


-- >>> evalt $ Pred $ Sum (B True) (Num 1)
-- >>> evalt $ If (Iszero $ Num 0) (Num 1) (B True) 

evalt :: EAB -> EAB
evalt exp@(Sum e1 e2) = case (vt [] e1 TNum, vt [] e2 TNum) of
                          (True, True) -> evals exp
                          _ -> error "Error de tipado o por existencia de variables libres."
evalt exp@(Prod e1 e2) = case (vt [] e1 TNum, vt [] e2 TNum) of
                           (True, True) -> evals exp
                           _ -> error "Error de tipado o por existencia de variables libres."
evalt e@(Neg e1) = case vt [] e1 TNum of
                     (True) -> evals e
                     _ -> error "Error de tipado o por existencia de variables libres." 

evalt e@(Pred e1) = case vt [] e1 TNum of
                      (True) -> evals e
                      _ -> error "Error de tipado o por existencia de variables libres." 
  
evalt e@(Suc e1) = case vt [] e1 TNum of
                     (True) -> evals e
                     _ -> error "Error de tipado o por existencia de variables libres." 

evalt e@(And e1 e2) = case (vt [] e1 TBool, vt [] e2 TBool) of
                        (True, True) -> evals e
                        _ -> error "Error de tipado o por existencia de variables libres." 

evalt e@(Or e1 e2) = case (vt [] e1 TBool, vt [] e2 TBool) of
                      (True, True) -> evals e
                      _ -> error "Error de tipado o por existencia de variables libres." 

evalt e@(Not e1) = case vt [] e1 TBool of
                     (True) -> evals e
                     _ -> error "Error de tipado o por existencia de variables libres." 

evalt e@(Iszero e1) = case vt [] e1 TNum of
                        (True) -> evals e
                        _ -> error "Error de tipado o por existencia de variables libres." 

evalt e@(If e1 e2 e3)
  | vt [] e1 TBool &&
    ((vt [] e2 TBool && vt [] e3 TBool) || (vt [] e2 TNum && vt [] e3 TNum)) = evals e
  | otherwise = error "Error de tipado o por existencia de variables libres." 
  
eval e@(Let e1 abs@(Abs x e2)) 
  | vt [] e1 TBool = if vt [(x, TBool)] e TBool || vt [(x, TBool)] e TNum
                     then evals e
                     else error "Error de tipado o por existencia de variables libres."
  | vt [] e1 TNum = if vt [(x, TNum)] e TBool || vt [(x, TNum)] e TNum
                    then evals e
                    else error "Error de tipado o por existencia de variables libres."
evalt e = error "Error de sintaxis."   
                      



