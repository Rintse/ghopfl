module Preprocess.Builtins where

import Data.Bifunctor (second)
import qualified Preprocess.Builtins.Conat as BConat (builtins)
import qualified Preprocess.Builtins.DelayedResult as BDelRes (builtins)
import qualified Preprocess.Builtins.Lists as BLists (builtins)
import qualified Preprocess.Builtins.Math as BMath (builtins)
import qualified Preprocess.Builtins.Func as BFunc (builtins)
import qualified Preprocess.Builtins.Random as BRandom (builtins)
import qualified Preprocess.Builtins.Stream as BStream (builtins)
import Syntax.Parse (parseExp)
import Syntax.Exp.Abs

-- Add prefixes to the builtins
builtinsRaw =
    BFunc.builtins
        ++ BMath.builtins
        ++ BStream.builtins
        ++ BConat.builtins
        ++ BRandom.builtins
        ++ BDelRes.builtins
        ++ BLists.builtins

parseBuiltins :: [Assignment]
parseBuiltins = do
    let toAssignment (name, text) = Assign (Ident name) (TSub "") (parseExp text)
    map toAssignment builtinsRaw
