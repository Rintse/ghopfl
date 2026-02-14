-- Defines the values for the big-step semantics
-- implemented in the Evaluation module
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Semantics.Values where

import Syntax.Expression
import Syntax.Number
import qualified Syntax.Exp.Abs as Raw

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

-- Result values
data Value
    = VSingle
    | VVal Number
    | VFalse
    | VTrue
    | VPair Exp Exp
    | VList [Exp]
    | VIn Exp
    | VInL Exp
    | VInR Exp
    | VNext Exp
    | VBox Environment Exp
    | VOut Exp
    | VThunk Exp
    | -- Unevaluated next
      VUNext Exp
    | -- Evaluated results
      VEPair Value Value
    | VEBox Value
    | VENext Value
    | VEIn Value
    | VEInL Value
    | VEInR Value
    deriving (Eq, Ord, Show, Read)

makeBaseFunctor ''Value

toExp :: Value -> Exp
toExp (VVal v) = Val v
toExp VTrue = BTrue
toExp VFalse = BFalse
toExp (VIn e) = In e
toExp (VInL e) = InL e
toExp (VInR e) = InR e
toExp (VThunk e) = e
toExp (VPair e1 e2) = Pair e1 e2
toExp (VNext e) = Next e
toExp (VOut e) = Out e
toExp (VBox l e) = Box l e
toExp (VEPair e1 e2) = Pair (toExp e1) (toExp e2)
toExp (VEInL e) = InL $ toExp e
toExp (VEInR e) = InR $ toExp e
toExp VSingle = Single
-- Should never be reached
toExp e = undefined
