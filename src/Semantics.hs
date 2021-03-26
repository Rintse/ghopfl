{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Small step semantics for guarded HOPFL
module Semantics where

import Substitution
import Syntax.Abs
import Syntax.ErrM
import Syntax.Fail
import Values
import Treeify
import VerbPrint

import Data.HashMap.Lazy as HM
import Data.Bifunctor
import Control.Applicative
import Debug.Trace
import Control.Monad.Reader
import Control.Monad.State

-- Evaluates a program given a maximum eval depth, and environment
-- To be called from Main.hs
evaluate :: Bool -> Exp -> Integer -> [Double] -> Environment -> IO ()
evaluate v prog n s env = do
    putStrV v ("Evaluating with random draws: " ++ show s)
    putStrV v ("Using the environment: " ++ show (mkEnv env))
    putStrV v ("Up to depth: " ++ show n ++ "\n")
    
    let startEnv = (mkEnv env, n)
    let startDraws = (1.0, s)
    let r = runStateT (runReaderT (runSem (evalExp prog)) startEnv) startDraws
    
    case r of
        Bad s -> do
            putStrLn "Evaluation failed:"
            putStrLn s
        Ok (s, (w,_)) -> putStrLn $
            "Evaluation result (with density " ++ show w ++ "): \n" ++ show s

-- Environment as hashmap
type Env = HashMap String Exp

-- Transform the environment AST into a hashmap
mkEnv :: Environment -> Env
mkEnv (Env e) = fromList $ fmap mkAssign e
    where mkAssign (Assign (Ident x) exp) = (x, exp)

-- Probability density function of the gaussian distribution
pdfNorm :: (Double, Double) -> Double -> Double
pdfNorm (m,v) c = let sd = sqrt v in -- variance is σ^2
    (1 / (sd * sqrt (2 * pi))) * exp (-0.5 * (((c - m) / sd) ^^ 2)) 

-- A monad containing:
--   - A reader with (a map from vars to their values, and the evaluation depth)
--   - A State with (a list of random draws, and the density of this execution)
--   - An error monad to express evaluation failure
newtype SemEnv a = SemEnv {
    runSem :: ReaderT (Env, Integer) (StateT (Double, [Double]) Err) a
} deriving (    Functor, Applicative, Monad,
                MonadReader (Env, Integer),
                MonadState (Double, [Double])   )

-- Evaluation function: 
-- Takes an AST and calculates the result of the program using big step semantics
evalExp :: Exp -> SemEnv Value

-- Variables
evalExp exp@(Var (Ident v)) = do
    val <- asks (\(x,_) -> HM.lookup v x)
    case val of
        Just e  -> evalExp e
        Nothing -> error $ "Undefined free variable: " ++ show v

-- Values (reals)
evalExp exp@(Val v) = return $ VVal v

-- Later modality
-- Do no allow calculation past n (param) nexts
evalExp exp@(Next e) = do
    depth <- asks snd
    if depth == 0 then return $ VNext e
    else local (second $ subtract 1) (VNext . toExp <$> evalExp e)

-- Put into fixpoint
evalExp exp@(In e) = return $ VIn e

-- Extract from fixpoint
evalExp exp@(Out e) = do
    r <- evalExp e
    case r of
        VIn v -> evalExp v
        _ -> return $ VOut exp

-- Function application
evalExp exp@(App e1 e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case r1 of
        VThunk (Abstr l (Ident x) e) -> evalExp $ substitute e x $ toExp r2
        _ -> error $ " Application on non-function:\n" ++ treeTerm exp

-- Delayed function application
evalExp exp@(LApp e1 _ e2) = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VNext t, VNext s) -> do
            evalExp $ Next $ App t s
        _ -> error $ "Invalid arguments to LApp:\n" ++ treeTerm exp

-- Pair creation
evalExp exp@(Pair e1 e2) = return $ VPair e1 e2

-- First projection
evalExp exp@(Fst e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> evalExp v1
        _ -> error $ "Took fst of non-pair " ++ treeTerm exp

-- Second projection
evalExp exp@(Snd e) = do
    r <- evalExp e
    case r of
        VPair v1 v2 -> evalExp v2
        _ -> error $ "Took snd of non-pair " ++ treeTerm exp

-- Normal distribtion sampling
evalExp exp@(Norm e) = do
    r <- evalExp e
    case r of -- Todo this case needed?
        VPair e1 e2 -> do
            r1 <- evalExp e1
            r2 <- evalExp e2
            case (r1,r2) of
                (VVal m, VVal v) -> performDraw m v
                _ -> error "Normal pair does not contain reals"
        _ -> error $ "Normal argument not a pair: \n" ++ treeTerm exp

-- If then else
evalExp exp@(Ite b e1 e2) = do
    rb <- evalExp b
    case rb of
        VBVal True  -> evalExp e1
        VBVal False -> evalExp e2
        _ -> error $ "If with non boolean condition:\n" ++ treeTerm exp

-- Coproduct injection
evalExp exp@(InL e) = return $ VInL e
evalExp exp@(InR e) = return $ VInR e

-- Matching coproducts
evalExp exp@(Match e (Ident x1) e1 (Ident x2) e2) = do
    re <- evalExp e
    case re of
        VInL l -> do
            l2 <- evalExp l
            evalExp $ substitute e1 x1 $ toExp l2
        VInR r -> do
            r2 <- evalExp r
            evalExp $ substitute e2 x2 $ toExp r2
        _       -> error $ "Match on non-coproduct:\n" ++ treeTerm exp

-- Function abstraction
evalExp exp@(Abstr l x e) = return $ VThunk exp

-- Recursion
evalExp exp@(Rec (Ident x) e) = evalExp $ substitute e x $ recName (Next exp)

-- Boolean and arithmetic expressions
evalExp exp = case exp of
    BVal v      -> return $ VBVal $ toBool v
    Val v       -> return $ VVal v
    Add e1 e2   -> evalAExp e1 (+) e2
    Sub e1 e2   -> evalAExp e1 (-) e2
    Mul e1 e2   -> evalAExp e1 (*) e2
    Div e1 e2   -> evalAExp e1 (/) e2

    Not _ e     -> evalBExp1 not e
    And e1 _ e2 -> evalBExp e1 (&&) e2
    Or e1 _ e2  -> evalBExp e1 (||) e2

    Eq e1 e2    -> evalRelop e1 (==) e2
    Lt e1 e2    -> evalRelop e1 (<) e2
    Gt e1 e2    -> evalRelop e1 (>) e2
    Leq e1 _ e2 -> evalRelop e1 (<=) e2
    Geq e1 _ e2 -> evalRelop e1 (>=) e2

-- HELPERS
-- Evaluates a binary arithmetic operation
evalAExp :: Exp -> (Double -> Double -> Double) -> Exp -> SemEnv Value
evalAExp e1 op e2 = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VVal d1, VVal d2) -> return $ VVal $ op d1 d2
        _ -> error "Non-real arguments to arithmetic operator"

-- Evaluates a binary boolean operation
evalBExp :: Exp -> (Bool -> Bool -> Bool) -> Exp -> SemEnv Value
evalBExp e1 op e2 = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VBVal b1, VBVal b2) -> return $ VBVal $ op b1 b2
        _ -> error "Non-bool arguments to boolean operator"

-- Evaluates a unary boolean operation
evalBExp1 :: (Bool -> Bool) -> Exp -> SemEnv Value
evalBExp1 op e = do
    r <- evalExp e
    case r of
        (VBVal b) -> return $ VBVal $ op b
        _ -> error "Non-bool arguments to boolean"

-- Evaluates a relative operator
evalRelop :: Exp -> (Double -> Double -> Bool) -> Exp -> SemEnv Value
evalRelop e1 op e2 = do
    r1 <- evalExp e1
    r2 <- evalExp e2
    case (r1, r2) of
        (VVal d1, VVal d2) -> return $ VBVal $ op d1 d2
        _ -> error $ "Non real arguments to relative operator:\n" ++
            treeValue r1 ++ "\n" ++ treeValue r2

performDraw :: Double -> Double -> SemEnv Value
performDraw m v = do
    env <- get
    case snd env of
        (c:l) -> do -- Todo: rename l2
            let density = pdfNorm (m,v) c in 
                if isNaN density
                then error "PDF not defined\n"
                else modify (\(w, _) -> (w * density, l))
            return $ VVal c
        _ -> error $ "Draws list too small (" ++ show m ++ ", " ++ show v ++ ")"

