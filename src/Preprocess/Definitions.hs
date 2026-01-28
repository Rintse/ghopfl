{-# LANGUAGE LambdaCase #-}

module Preprocess.Definitions where

import Control.Monad (unless)
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.Functor.Foldable.TH
import qualified Data.Set as Set
import Debug.Trace
import Preprocess.Builtins (parseBuiltins)
import Semantics.Substitution
import Syntax.AbsF
import Syntax.Raw.Abs
import System.Exit

-- Perform a single definition substitution
defSub :: Exp -> Reader Assignment Exp
defSub exp = do
  a@(Assign (Ident s) o t) <- ask
  let doList (Assign x o t) = Assign x o $ runDef t a
  ( anaM $ \case
      Prev (Env l) e -> return $ PrevF (Env $ map doList l) e
      Box (Env l) e -> return $ BoxF (Env $ map doList l) e
      EList (List l) -> return $ EListF $ List $ map (`runDef` a) l
      e@(Var (Ident x)) ->
        if x == s
          then return $ project t
          else return $ project e
      other -> return $ project other
    )
    exp

-- Runs definition substitution inside a reader monad
runDef :: Exp -> Assignment -> Exp
runDef e = runReader (defSub e)

-- Perform a definition substitution all following definitions
inDef :: [Assignment] -> Assignment -> [Assignment]
inDef l a = map (\(Assign x o t) -> Assign x o (runDef t a)) l

-- Perform all definition subs in the definitions after it
inDefs :: [Assignment] -> [Assignment]
inDefs [] = []
inDefs [a] = [a]
inDefs (a : l) = a : inDefs (inDef l a)

toSet :: [Assignment] -> Set.Set String
toSet l = Set.fromList $ map (\(Assign (Ident x) _ _) -> x) l

-- Perform definition substitutions for a list of defs
handleDefs :: Prg -> IO Exp
handleDefs (Prog e) = return $ foldl runDef e $ inDefs builtins
handleDefs p@(DefProg (Env l) e) = do
  let builtins = parseBuiltins
  let builtinNames = Set.fromList $ map fst builtins
  let envNames = Set.fromList $ map (\(Assign (Ident s) a b) -> s) l
  let usedBuiltinNames = Set.intersection builtinNames envNames

  unless
    (null usedBuiltinNames)
    ( do
        putStrLn "Error. Used reserved name of builtin:"
        mapM_ putStrLn (Set.toList usedBuiltinNames)
        exitFailure
    )

  -- Expand programmers own definitions
  let customDefs = foldl runDef e $ inDefs l

  -- Expand the builtin definitions
  return $ foldl runDef customDefs $ inDefs builtins

