-- Haskell module generated by the BNF converter

module Syntax.Raw.Skel where

import qualified Syntax.Raw.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Syntax.Raw.Abs.Ident -> Result
transIdent x = case x of
  Syntax.Raw.Abs.Ident string -> failure x
transLam :: Syntax.Raw.Abs.Lam -> Result
transLam x = case x of
  Syntax.Raw.Abs.Lam string -> failure x
transConj :: Syntax.Raw.Abs.Conj -> Result
transConj x = case x of
  Syntax.Raw.Abs.Conj string -> failure x
transDisj :: Syntax.Raw.Abs.Disj -> Result
transDisj x = case x of
  Syntax.Raw.Abs.Disj string -> failure x
transTNot :: Syntax.Raw.Abs.TNot -> Result
transTNot x = case x of
  Syntax.Raw.Abs.TNot string -> failure x
transTLeq :: Syntax.Raw.Abs.TLeq -> Result
transTLeq x = case x of
  Syntax.Raw.Abs.TLeq string -> failure x
transTGeq :: Syntax.Raw.Abs.TGeq -> Result
transTGeq x = case x of
  Syntax.Raw.Abs.TGeq string -> failure x
transTLApp :: Syntax.Raw.Abs.TLApp -> Result
transTLApp x = case x of
  Syntax.Raw.Abs.TLApp string -> failure x
transTSub :: Syntax.Raw.Abs.TSub -> Result
transTSub x = case x of
  Syntax.Raw.Abs.TSub string -> failure x
transTMatch :: Syntax.Raw.Abs.TMatch -> Result
transTMatch x = case x of
  Syntax.Raw.Abs.TMatch string -> failure x
transTSingle :: Syntax.Raw.Abs.TSingle -> Result
transTSingle x = case x of
  Syntax.Raw.Abs.TSingle string -> failure x
transBConst :: Syntax.Raw.Abs.BConst -> Result
transBConst x = case x of
  Syntax.Raw.Abs.BTrue -> failure x
  Syntax.Raw.Abs.BFalse -> failure x
transExp :: Syntax.Raw.Abs.Exp -> Result
transExp x = case x of
  Syntax.Raw.Abs.Single tsingle -> failure x
  Syntax.Raw.Abs.Var ident -> failure x
  Syntax.Raw.Abs.DVal double -> failure x
  Syntax.Raw.Abs.IVal integer -> failure x
  Syntax.Raw.Abs.BVal bconst -> failure x
  Syntax.Raw.Abs.EList lst -> failure x
  Syntax.Raw.Abs.Pair exp1 exp2 -> failure x
  Syntax.Raw.Abs.Next exp -> failure x
  Syntax.Raw.Abs.Prev environment exp -> failure x
  Syntax.Raw.Abs.PrevE exp -> failure x
  Syntax.Raw.Abs.PrevI exp -> failure x
  Syntax.Raw.Abs.Box environment exp -> failure x
  Syntax.Raw.Abs.BoxI exp -> failure x
  Syntax.Raw.Abs.Unbox exp -> failure x
  Syntax.Raw.Abs.Force exp -> failure x
  Syntax.Raw.Abs.Rand -> failure x
  Syntax.Raw.Abs.In exp -> failure x
  Syntax.Raw.Abs.Out exp -> failure x
  Syntax.Raw.Abs.Fst exp -> failure x
  Syntax.Raw.Abs.Snd exp -> failure x
  Syntax.Raw.Abs.InL exp -> failure x
  Syntax.Raw.Abs.InR exp -> failure x
  Syntax.Raw.Abs.ListHead exp -> failure x
  Syntax.Raw.Abs.ListTail exp -> failure x
  Syntax.Raw.Abs.ListNull exp -> failure x
  Syntax.Raw.Abs.ListLength exp -> failure x
  Syntax.Raw.Abs.ListFold exp1 exp2 exp3 -> failure x
  Syntax.Raw.Abs.ListMap exp1 exp2 -> failure x
  Syntax.Raw.Abs.ListElem exp1 exp2 -> failure x
  Syntax.Raw.Abs.ListTake exp1 exp2 -> failure x
  Syntax.Raw.Abs.ListDrop exp1 exp2 -> failure x
  Syntax.Raw.Abs.App exp1 exp2 -> failure x
  Syntax.Raw.Abs.LApp exp1 tlapp exp2 -> failure x
  Syntax.Raw.Abs.ListIndex exp1 exp2 -> failure x
  Syntax.Raw.Abs.ListCons exp1 exp2 -> failure x
  Syntax.Raw.Abs.ListAppend exp1 exp2 -> failure x
  Syntax.Raw.Abs.Min exp -> failure x
  Syntax.Raw.Abs.Pow exp1 exp2 -> failure x
  Syntax.Raw.Abs.Mul exp1 exp2 -> failure x
  Syntax.Raw.Abs.Div exp1 exp2 -> failure x
  Syntax.Raw.Abs.Mod exp1 exp2 -> failure x
  Syntax.Raw.Abs.Add exp1 exp2 -> failure x
  Syntax.Raw.Abs.Sub exp1 exp2 -> failure x
  Syntax.Raw.Abs.Eq exp1 exp2 -> failure x
  Syntax.Raw.Abs.Lt exp1 exp2 -> failure x
  Syntax.Raw.Abs.Gt exp1 exp2 -> failure x
  Syntax.Raw.Abs.Leq exp1 tleq exp2 -> failure x
  Syntax.Raw.Abs.Geq exp1 tgeq exp2 -> failure x
  Syntax.Raw.Abs.Not tnot exp -> failure x
  Syntax.Raw.Abs.And exp1 conj exp2 -> failure x
  Syntax.Raw.Abs.Or exp1 disj exp2 -> failure x
  Syntax.Raw.Abs.Norm exp -> failure x
  Syntax.Raw.Abs.Ite exp1 exp2 exp3 -> failure x
  Syntax.Raw.Abs.Match exp1 ident1 tmatch1 exp2 ident2 tmatch2 exp3 -> failure x
  Syntax.Raw.Abs.Abstr lam ident exp -> failure x
  Syntax.Raw.Abs.Rec ident exp -> failure x
transLst :: Syntax.Raw.Abs.Lst -> Result
transLst x = case x of
  Syntax.Raw.Abs.List exps -> failure x
transPrg :: Syntax.Raw.Abs.Prg -> Result
transPrg x = case x of
  Syntax.Raw.Abs.DefProg environment exp -> failure x
  Syntax.Raw.Abs.Prog exp -> failure x
transEnvironment :: Syntax.Raw.Abs.Environment -> Result
transEnvironment x = case x of
  Syntax.Raw.Abs.Env assignments -> failure x
transAssignment :: Syntax.Raw.Abs.Assignment -> Result
transAssignment x = case x of
  Syntax.Raw.Abs.Assign ident tsub exp -> failure x

