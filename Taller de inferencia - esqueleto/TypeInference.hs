module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Env, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

infer' (VarExp x)     n = OK (n+1, (extendE emptyEnv "x" (TVar n), VarExp "x", (TVar n)))

infer' (ZeroExp)      n = OK (n, (emptyEnv, ZeroExp, TNat))

infer' (LamExp x a e) n = 
	case infer' e n of 
		OK (n', (env',e',t')) -> 
			OK (m, (removeE env' x, LamExp x p e',(TFun p t')))
			where
				p = if (elem x (domainE env')) then evalE env' x else (TVar n') 
				m = if (elem x (domainE env')) then n'+1 else n'
		res@(Error _ ) -> res




--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
