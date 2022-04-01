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

--Segun yo este es el constanf folding, el del examen, el 2,de un programa es decir
--solo evalua constantes y boolenaos 2.2
--Un estado bloqueado es que ya no tiene otro paso de evaluación, es decir, ya llegaste
--a un valor o tienes una variable libre (2+true)
evals :: EAB -> EAB 
evals _ = error "Implementar"

--Hace lo mismo que evals pero maneja errores 2.3
--si no llegaste a un valor, mandar un error
--Como (2+true) -> error
--Como (true-2) -> error
--mandar a llamar la fun evals y hacer un case of del resultado de llamar evals
--dependiendo de ese resultado regresar nuestro error personalizado
eval :: EAB -> EAB
eval _ = error "Implementar"

--data Type = () -- Definir los tipos de EAB, es decir booleanos y naturales
--type Ctx = () -- Definir un sinomo para los contextos, es decir, listas de tuplas
-- necesitmos definir el contexto vacio -- 17

data Type = TNum | TBool deriving Show--nuestro Type solo pueden ser los dos tipos TNum y TBool
-- Num 5 :- TNat y B False :-TBool 

type Ctx = [(String,Type)] deriving Show-- ->  :t (("c",TNum),("x",TBool)) 7u7
--vt :: Ctx -> EAB -> Type -> Bool
--vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"
