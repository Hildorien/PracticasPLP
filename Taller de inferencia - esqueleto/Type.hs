module Type(Type(..), fv,
			Symbol,
            Env, emptyEnv, extendE, removeE, evalE, joinE, domainE,
            Subst, emptySubst, extendS, Substitutable, (<.>))
where

-----------
-- Types --
-----------

data Type = TVar Int | TNat | TBool | TFun Type Type
instance Show Type where
    show (TVar n) = "t" ++ show n
    show TNat = "Nat"
    show TBool = "Bool"
    show (TFun t1 t2) = (parensFun t1 $ show t1) ++ " -> " ++ show t2
        where parensFun (TFun _ _) = \s -> "(" ++ s ++ ")"
              parensFun _ = id


fv :: Type -> [Int]
fv TNat = []
fv TBool = []
fv (TVar n) = [n]
fv (TFun t1 t2) = fv t1 ++ fv t2

------------------
-- Environments --
------------------

type Symbol = String

newtype Env = E [(Symbol,  Type)] deriving Show

emptyEnv :: Env
emptyEnv = E []

extendE :: Env -> Symbol -> Type -> Env
extendE (E env) x t = E ((x,t):env)

removeE :: Env -> Symbol -> Env
removeE (E env) x = E (filter ((/=x) . fst) env)

evalE :: Env -> Symbol -> Type
evalE (E env) x = case lookup x env of
                    (Just t) -> t
                    Nothing -> error $ "Environment does not include " ++ show x

joinE :: [Env] -> Env
joinE = env . concat . map unEnv
    where env = E
          unEnv (E e) = e

domainE :: Env -> [Symbol]
domainE (E env) = map fst env

-------------------
-- Substitutions --
-------------------

newtype Subst = S (Int -> Type)

emptySubst :: Subst
emptySubst = S (\n -> TVar n)

extendS :: Int -> Type -> Subst -> Subst
extendS n t (S subs) = S (\m -> if m == n then t else subs m)

class Substitutable a where
    infixr 5 <.>
    (<.>) :: Subst -> a -> a

instance Substitutable Type where 
    (<.>) _     TNat        = TNat
    (<.>) _     TBool       = TBool
    (<.>) s     (TFun t1 t2)= TFun (s <.> t1) (s <.> t2)
    (<.>) (S s) (TVar n)    = unfold s n
        where unfold f m = case f m of
                            tVar@(TVar m') -> if m == m' then tVar else (S f) <.> tVar
                            tOther -> (S f) <.> tOther


instance Substitutable Env where
    (<.>) s (E env) = E (map (\(x,t)->(x, s <.> t)) env)

instance Substitutable Subst where
    (<.>) s (S s2) = S (\n -> s <.> s2 n)

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    (<.>) s (x,y) = (s <.> x, s <.> y)

instance Substitutable a => Substitutable [a] where
    (<.>) s = map (s <.>)