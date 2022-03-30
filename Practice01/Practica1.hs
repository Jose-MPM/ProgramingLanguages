module EAB where

data EAB = Exp -- Definir la sintaxis abstracta de nuestro lenguaje EAB

eval1 :: EAB -> EAB
eval1 _ = error "Implementar"

evals :: EAB -> EAB
evals _ = error "Implementar"

eval :: EAB -> EAB
eval _ = error "Implementar"

data Type = () -- Definir los tipos de EAB
type Ctx = () -- Definir un sinomo para los contextos

vt :: Ctx -> EAB -> Type -> Bool
vt _ _ _ = error "Implementar"

evalt :: EAB -> EAB
evalt _ = error "Implementar"