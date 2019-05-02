module Exp (Exp(..), PlainExp, AnnotExp, foldPrimExp, foldExp)
where

import Type

data Exp a = VarExp Symbol |
             ZeroExp |
             SuccExp (Exp a)|
             PredExp (Exp a) |
             IsZeroExp (Exp a) |
             TrueExp |
             FalseExp |
             IfExp (Exp a) (Exp a) (Exp a) |
             LamExp Symbol a (Exp a) |
             AppExp (Exp a) (Exp a) deriving Show

type PlainExp = Exp ()
type AnnotExp = Exp Type

instance Substitutable a => Substitutable (Exp a) where
    (<.>) subst = foldExp VarExp
                          ZeroExp
                          SuccExp
                          PredExp
                          IsZeroExp
                          TrueExp
                          FalseExp
                          IfExp
                          (\x t e -> LamExp x (subst <.> t) e)
                          AppExp

foldPrimExp :: (Symbol -> a)
               -> a
               -> (Exp b -> a -> a)
               -> (Exp b -> a -> a)
               -> (Exp b -> a -> a)
               -> a
               -> a
               -> (Exp b -> Exp b -> Exp b -> a -> a -> a -> a)
               -> (Exp b -> Symbol -> b -> a -> a)
               -> (Exp b -> Exp b -> a -> a -> a)
               -> Exp b -> a
foldPrimExp fVar _ _ _ _ _ _ _ _ _ (VarExp x) = fVar x
foldPrimExp _ fZero _ _ _ _ _ _ _ _ (ZeroExp) = fZero
foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp (SuccExp e)
  = fSucc e $ foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp e
foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp (PredExp e)
  = fPred e $ foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp e
foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp (IsZeroExp e)
  = fIsZero e $ foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp e
foldPrimExp _ _ _ _ _ fTrue _ _ _ _ (TrueExp) = fTrue
foldPrimExp _ _ _ _ _ _ fFalse _ _ _ (FalseExp) = fFalse
foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp (IfExp e1 e2 e3)
  = fIf e1 e2 e3 (rec e1) (rec e2) (rec e3)
    where rec = foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp
foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp (LamExp x b e)
  = fLam e x b $ foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp e
foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp (AppExp e1 e2)
  = fApp e1 e2 (rec e1) (rec e2)
    where rec = foldPrimExp fVar fZero fSucc fPred fIsZero fTrue fFalse fIf fLam fApp

foldExp :: (Symbol -> a)
           -> a
           -> (a -> a)
           -> (a -> a)
           -> (a -> a)
           -> a
           -> a
           -> (a -> a -> a -> a)
           -> (Symbol -> b -> a -> a)
           -> (a -> a -> a)
           -> Exp b -> a
foldExp fVar fZero fSucc fPred fIfZero fTrue fFalse fIf fLam fApp =
    foldPrimExp fVar fZero (const fSucc) (const fPred) (const fIfZero) fTrue fFalse
                (const $ const $ const fIf) (const fLam) (const $ const fApp)
