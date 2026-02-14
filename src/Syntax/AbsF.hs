{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax.AbsF where

import Syntax.Exp.Abs

import Data.Functor.Foldable.TH

makeBaseFunctor ''Exp
