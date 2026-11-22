module Preprocess.Builtins where

import Data.Bifunctor (second)
import Data.HashMap.Lazy as HM (HashMap, fromList, empty)
-- import Preprocess.AnnotateVars (annotateVars)
import Syntax.Expression
import Syntax.Parse (parseExp)
import qualified Data.Set as Set (Set, fromList)
import System.Directory (listDirectory)

-- builtins :: IO (HM.HashMap String Exp)
-- builtins = fromList $ map (second (annotateVars . parseExp)) builtinsRaw

parseBuiltins:: IO (HashMap String String)
parseBuiltins = do
    let sourceFiles = listDirectory
    return HM.empty

-- builtinNames = Set.fromList $ map fst builtinsRaw
