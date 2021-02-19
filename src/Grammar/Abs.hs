-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammar.Abs where

import Prelude (Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Lam = Lam String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Prod = Prod String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype To = To String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Next = Next String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype Napp = Napp String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Typ
    = TReal | TNext Next Typ | TPRod Typ Prod Typ | TFun Typ To Typ
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = Var Ident
    | Val Integer
    | Rec Ident Exp
    | Abstr Lam Ident Exp
    | App Exp Exp
    | NApp Exp Napp Exp
    | Pair Exp Exp
    | Fst Exp
    | Snd Exp
    | Typed Exp Typ
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Assignment = Assign Ident Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Environment = Env [Assignment]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

