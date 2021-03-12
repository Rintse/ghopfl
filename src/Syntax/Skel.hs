-- Haskell module generated by the BNF converter

module Syntax.Skel where

import qualified Syntax.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Syntax.Abs.Ident -> Result
transIdent x = case x of
  Syntax.Abs.Ident string -> failure x
transLam :: Syntax.Abs.Lam -> Result
transLam x = case x of
  Syntax.Abs.Lam string -> failure x
transConj :: Syntax.Abs.Conj -> Result
transConj x = case x of
  Syntax.Abs.Conj string -> failure x
transDisj :: Syntax.Abs.Disj -> Result
transDisj x = case x of
  Syntax.Abs.Disj string -> failure x
transTNot :: Syntax.Abs.TNot -> Result
transTNot x = case x of
  Syntax.Abs.TNot string -> failure x
transTLeq :: Syntax.Abs.TLeq -> Result
transTLeq x = case x of
  Syntax.Abs.TLeq string -> failure x
transTGeq :: Syntax.Abs.TGeq -> Result
transTGeq x = case x of
  Syntax.Abs.TGeq string -> failure x
transTLApp :: Syntax.Abs.TLApp -> Result
transTLApp x = case x of
  Syntax.Abs.TLApp string -> failure x
transBConst :: Syntax.Abs.BConst -> Result
transBConst x = case x of
  Syntax.Abs.BTrue -> failure x
  Syntax.Abs.BFalse -> failure x
transExp :: Syntax.Abs.Exp -> Result
transExp x = case x of
  Syntax.Abs.Var ident -> failure x
  Syntax.Abs.Val double -> failure x
  Syntax.Abs.BVal bconst -> failure x
  Syntax.Abs.Next exp -> failure x
  Syntax.Abs.In exp -> failure x
  Syntax.Abs.Out exp -> failure x
  Syntax.Abs.Fst exp -> failure x
  Syntax.Abs.Snd exp -> failure x
  Syntax.Abs.InL exp -> failure x
  Syntax.Abs.InR exp -> failure x
  Syntax.Abs.App exp1 exp2 -> failure x
  Syntax.Abs.LApp exp1 tlapp exp2 -> failure x
  Syntax.Abs.Mul exp1 exp2 -> failure x
  Syntax.Abs.Div exp1 exp2 -> failure x
  Syntax.Abs.Add exp1 exp2 -> failure x
  Syntax.Abs.Sub exp1 exp2 -> failure x
  Syntax.Abs.Eq exp1 exp2 -> failure x
  Syntax.Abs.Lt exp1 exp2 -> failure x
  Syntax.Abs.Gt exp1 exp2 -> failure x
  Syntax.Abs.Leq exp1 tleq exp2 -> failure x
  Syntax.Abs.Geq exp1 tgeq exp2 -> failure x
  Syntax.Abs.Not tnot exp -> failure x
  Syntax.Abs.And exp1 conj exp2 -> failure x
  Syntax.Abs.Or exp1 disj exp2 -> failure x
  Syntax.Abs.Pair exp1 exp2 -> failure x
  Syntax.Abs.Norm exp -> failure x
  Syntax.Abs.Ite exp1 exp2 exp3 -> failure x
  Syntax.Abs.Case exp1 ident1 exp2 ident2 exp3 -> failure x
  Syntax.Abs.Abstr lam ident exp -> failure x
  Syntax.Abs.Rec ident exp -> failure x
transAssignment :: Syntax.Abs.Assignment -> Result
transAssignment x = case x of
  Syntax.Abs.Assign ident exp -> failure x
transEnvironment :: Syntax.Abs.Environment -> Result
transEnvironment x = case x of
  Syntax.Abs.Env assignments -> failure x

