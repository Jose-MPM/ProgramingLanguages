module EAB where

--debe de ser como el de las notas
data EAB = Var String
         | Num Int
         | Bool Bool --f = Bool False --t = Bool True 
         | Sum EAB EAB
         | Prod EAB EAB
         | Neg EAB
         | Pred EAB
         | Suc EAB
         | And EAB EAB -- And f t -> And (Bool False) (Bool True)
         | Or EAB EAB
         | Not EAB
         | IsZero EAB 
         | If EAB EAB EAB
         | Let EAB EAB
--         | MatchNat EAB EAB EAB
--         | GT EAB EAB
         | Abs String EAB  --Operador para el ligado de variables
        -- y delegarle la tarea de revisar las cosas especiales
         deriving (Show)

eval1 :: EAB -> EAB    
eval1 _ = error "Implementar"

--Segun yo este es el constanf folding, el del examen, el 2, es decir
--solo evalua constantes y boolenaos 2.2
evals :: EAB -> EAB 
evals _ = error "Implementar"

--Hace lo mismo que evals pero maneja errores 2.3
--Como (2+true) -> error
--Como (true-2) -> error
eval :: EAB -> EAB
eval _ = error "Implementar"

--data Type = () -- Definir los tipos de EAB
--type Ctx = () -- Definir un sinomo para los contextos

--vt :: Ctx -> EAB -> Type -> Bool
--vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"
