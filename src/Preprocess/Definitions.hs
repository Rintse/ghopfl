{-# LANGUAGE LambdaCase #-}

module Preprocess.Definitions where

import Control.Monad (unless)
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.Functor.Foldable.TH
import qualified Data.Set as Set
import Debug.Trace
import Semantics.Substitution
import System.Exit
import Syntax.Expression

-- Perform a single definition substitution
defSub :: Exp -> Reader Assignment Exp
defSub exp = do
    a@(Assign (Ident s i1 _) t) <- ask
    let doList (Assign x t) = Assign x $ runDef t a
    let go e@(Var (Ident x i2 _)) = if x == s && i1 == i2
        then return $ project t
        else return $ project e
    let go (Prev (Env l) e) = return $ PrevF (Env $ map doList l) e
    let go (Box (Env l) e) = return $ BoxF (Env $ map doList l) e
    let go (LetIn (Env l) e) = return $ LetInF (Env $ map doList l) e
    let go other = return $ project other
    anaM go exp

-- Runs definition substitution inside a reader monad
runDef :: Exp -> Assignment -> Exp
runDef e = runReader (defSub e)

-- Perform a definition substitution all following definitions
inDef :: [Assignment] -> Assignment -> [Assignment]
inDef l a = map (\(Assign x t) -> Assign x (runDef t a)) l

-- Perform all definition subs in the definitions after it
inDefs :: [Assignment] -> [Assignment]
inDefs (a : l) = a : inDefs (inDef l a)
inDefs a = a

toSet :: [Assignment] -> Set.Set String
toSet l = Set.fromList $ map (\(Assign (Ident x _ _) _) -> x) l

handleLetIns :: Exp -> IO Exp
handleLetIns e = do
    let getName = map (\(Assign (Ident s _ _) b) -> s)
    -- let builtinNames = Set.fromList $ getName builtins
    -- let envNames = Set.fromList $ getName l
    -- let usedBuiltinNames = Set.intersection builtinNames envNames

    -- unless
    --     (null usedBuiltinNames)
    --     ( do
    --         putStrLn "Warning. Used reserved name:"
    --         mapM_ (putStrLn . ("- " ++)) (Set.toList usedBuiltinNames)
    --     )
    
    undefined

-- Perform definition substitutions for a list of defs
-- handleDefs :: Exp -> IO Exp
-- handleDefs (Exp e) = return $ foldl runDef e $ inDefs parseBuiltins
-- handleDefs p@(DefProg (Env l) e) = do
--     let builtins = parseBuiltins
--     let getName = map (\(Assign (Ident s) a b) -> s)
--     let builtinNames = Set.fromList $ getName builtins
--     let envNames = Set.fromList $ getName l
--     let usedBuiltinNames = Set.intersection builtinNames envNames
--
--     unless
--         (null usedBuiltinNames)
--         ( do
--             putStrLn "Warning. Used reserved name:"
--             mapM_ (putStrLn . ("- " ++)) (Set.toList usedBuiltinNames)
--         )
--
--     -- Expand programmers own definitions
--     let customDefs = foldl runDef e $ inDefs l
--     -- Expand the builtin definitions
--     return $ foldl runDef customDefs $ inDefs builtins
