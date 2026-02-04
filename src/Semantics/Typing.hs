module Semantics.Typing where

import Syntax.Expression
import Syntax.Types.Abs

-- https://en.wikipedia.org/wiki/Hindley–Milner_type_system#An_inference_algorithm

typeCheck :: Exp -> Ty
typeCheck e = Leaf Bool
