module Preprocess.Builtins where

import Data.Bifunctor (second)
import qualified Preprocess.Builtins.Other as BOther (builtins)
import qualified Preprocess.Builtins.Math as BMath (builtins)
import qualified Preprocess.Builtins.Stream as BStream (builtins)
import qualified Preprocess.Builtins.Conat as BConat (builtins)
import qualified Preprocess.Builtins.Random as BRandom (builtins)
import qualified Preprocess.Builtins.DelayedResult as BDelRes (builtins)
import Syntax.Raw.Abs
import Syntax.Parse (parseExp)

-- Add prefixes to the builtins
builtinsRaw =
  BOther.builtins
    ++ BMath.builtins
    ++ BStream.builtins
    ++ BConat.builtins
    ++ BRandom.builtins
    ++ BDelRes.builtins

parseBuiltins :: [(String, Exp)]
parseBuiltins = map (second parseExp) builtinsRaw
