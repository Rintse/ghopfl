module Preprocess.Builtins where

import Data.Bifunctor (second)
import Data.HashMap.Lazy as HM (HashMap, fromList)
-- import Preprocess.AnnotateVars (annotateVars)
import qualified Preprocess.Builtins.Conat as BConat (builtins)
import qualified Preprocess.Builtins.DelayedResult as BDelRes (builtins)
import qualified Preprocess.Builtins.Func as BFunc (builtins)
import qualified Preprocess.Builtins.Lists as BLists (builtins)
import qualified Preprocess.Builtins.Math as BMath (builtins)
import qualified Preprocess.Builtins.Random as BRandom (builtins)
import qualified Preprocess.Builtins.Stream as BStream (builtins)
import Syntax.Expression
import Syntax.Parse (parseExp)
import qualified Data.Set as Set (Set, fromList)

builtinsRaw =
    BFunc.builtins
        ++ BMath.builtins
        ++ BStream.builtins
        ++ BConat.builtins
        ++ BRandom.builtins
        ++ BDelRes.builtins
        ++ BLists.builtins

builtinNames :: Set.Set String
builtinNames = Set.fromList $ map fst builtinsRaw
