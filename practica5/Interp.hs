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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun params e
  | length params /= length (nub params) = Nothing
  | otherwise = Just (foldr Fun e params)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f args = Just (foldl App f args)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:xs) = Just (foldl op x xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] alternativa = desugar alternativa
desugarCond ((c, r) : cs) alternativa =
  If <$> desugar c <*> desugar r <*> desugarCond cs alternativa

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS es) = traverse desugar es >>= binaryOp Add
desugar (SubS es) = traverse desugar es >>= binaryOp Sub
desugar (NotS e) = Not <$> desugar e
desugar (LetS x e1 e2) = App <$> (Fun x <$> desugar e2) <*> desugar e1
desugar (LetStarS [] cuerpo) = desugar cuerpo
desugar (LetStarS ((x, e) : bs) cuerpo) = desugar (LetS x e (LetStarS bs cuerpo))
desugar (FunS params cuerpo) = desugar cuerpo >>= curryFun params
desugar (AppS f args) = traverse desugar args >>= \args' -> desugar f >>= \f' -> curryApp f' args'
desugar (IfS c t e) = If <$> desugar c <*> desugar t <*> desugar e
desugar (CondS [] _) = Nothing
desugar (CondS clausulas alternativa) = desugarCond clausulas alternativa
desugar (LetRecS f definicion cuerpo) =
  desugar (LetS f (AppS (IdS "Y") [FunS [f] definicion]) cuerpo)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv = lookup

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (ExprV e env) = bigStep env e >>= strict
strict v = Just v

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Id x) = lookupEnv x env
bigStep env (Fun x e) = Just (ClosureV x e env)
bigStep env (Add e1 e2) = NumV <$> ((+) <$> exigeNum env e1 <*> exigeNum env e2)
bigStep env (Sub e1 e2) = NumV . max 0 <$> ((-) <$> exigeNum env e1 <*> exigeNum env e2)
bigStep env (Not e) = BooleanV . not <$> (exige env e >>= aBool)
bigStep env (If c t e) = exige env c >>= aBool >>= bigStep env . elige t e
bigStep env (App f a) = exige env f >>= aplica env a

-- Funciones auxiliares:
exige :: Env -> ASA -> Maybe Value
exige env e = bigStep env e >>= strict

exigeNum :: Env -> ASA -> Maybe Int
exigeNum env e = exige env e >>= aNum

aNum :: Value -> Maybe Int
aNum (NumV n) = Just n
aNum _ = Nothing

aBool :: Value -> Maybe Bool
aBool (BooleanV b) = Just b
aBool _ = Nothing

elige :: ASA -> ASA -> Bool -> ASA
elige t _ True = t
elige _ e False = e

aplica :: Env -> ASA -> Value -> Maybe Value
aplica env a (ClosureV p cuerpo envF) = bigStep ((p, ExprV a env) : envF) cuerpo
aplica _ _ _ = Nothing
