module EAB where

--debe de ser como el de las notas
data EAB = Var String --
         | Num Int -- 
         | B Bool --f = B False --t = B True 
         | Sum EAB EAB
         | Prod EAB EAB
         | Neg EAB
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
eval1 :: EAB -> EAB

eval1 (Sum e1 e2) = case (e1,e2) of
                     (Num n, Num m) -> Num (n+m)
                     (Num n, e) -> Sum (Num n) (eval1 e)
                     (e1,e2) -> Sum (eval1 e1) e2
eval1 (Prod e1 e2) = case (e1,e2) of
                     (Num n, Num m) -> Num (n*m)
                     (Num n, e) -> Prod (Num n) (eval1 e)
                     (e1,e2) -> Prod (eval1 e1) e2
eval1 (Pred e1) = case e1 of
                    (Num n) -> Num (n-1)
                    (e) -> Pred (eval1 e)
eval1 (Suc e1) = case e1 of
                   (Num n) -> Num(n+1)
                   (e) -> Suc (eval1 e)
eval1 (Let e1 (Abs var ))                   
eval1 _ = e

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
-- necesitmos definir el contexto vacio.

--vt :: Ctx -> EAB -> Type -> Bool
--vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"
