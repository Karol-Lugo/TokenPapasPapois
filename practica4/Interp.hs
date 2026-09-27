module Interp where

import Grammars
import Data.List (nub)

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun params e
  | length params /= length (nub params) = Nothing
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e =
  case curryFun xs e of
             Just v  -> Just (Fun x v)
             Nothing -> Nothing

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)


-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:xs) = Just (foldl op x xs)



-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (NumS n)     = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (IdS x)      = Just (Id x)

desugar (AddS es) =
  case traverse desugar es of
    Nothing  -> Nothing
    Just es' -> binaryOp Add es'

desugar (SubS es) =
  case traverse desugar es of
    Nothing  -> Nothing
    Just es' -> binaryOp Sub es'

desugar (NotS e) =
  case desugar e of
    Nothing -> Nothing
    Just e' -> Just (Not e')

desugar (FunS params body) =
  case desugar body of
    Nothing    -> Nothing
    Just body' -> curryFun params body'

desugar (AppS f args) =
  case desugar f of
    Nothing -> Nothing
    Just f' ->
      case traverse desugar args of
        Nothing    -> Nothing
        Just args' -> curryApp f' args'

desugar (LetS x e1 e2) =
  case desugar e1 of
    Nothing -> Nothing
    Just e1' ->
      case desugar e2 of
        Nothing  -> Nothing
        Just e2' -> Just (App (Fun x e2') e1')

desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x, e1):bs) body) =
  case desugar e1 of
    Nothing -> Nothing
    Just e1' ->
      case desugar (LetStarS bs body) of
        Nothing   -> Nothing
        Just rest -> Just (App (Fun x rest) e1')
      
-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x env = lookup x env

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep _   (Num n)     = Just (NumV n)
bigStep _   (Boolean b) = Just (BooleanV b)
bigStep env (Id x)      = lookupEnv x env
bigStep env (Fun x e)   = Just (ClosureV x e env)

bigStep env (Add e1 e2) =
  case (bigStep env e1, bigStep env e2) of
    (Just (NumV n1), Just (NumV n2)) -> Just (NumV (n1 + n2))
    _ -> Nothing

bigStep env (Sub e1 e2) =
  case (bigStep env e1, bigStep env e2) of
    (Just (NumV n1), Just (NumV n2)) -> Just (NumV (n1 - n2))
    _ -> Nothing

bigStep env (Not e) =
  case bigStep env e of
    Just (BooleanV b) -> Just (BooleanV (not b))
    Just (NumV _)      -> Just (BooleanV False)
    _ -> Nothing

bigStep env (App e1 e2) =
  case bigStep env e1 of
    Just (ClosureV param body closureEnv) ->
      case bigStep env e2 of
        Just argVal -> bigStep ((param, argVal) : closureEnv) body
        Nothing -> Nothing
    _ -> Nothing
