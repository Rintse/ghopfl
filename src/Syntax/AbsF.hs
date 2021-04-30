{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.AbsF where

import Syntax.Raw.Abs

import Data.Functor.Foldable.TH
import Data.Functor.Foldable

makeBaseFunctor ''Exp
