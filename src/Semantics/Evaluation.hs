{-# LANGUAGE FlexibleContexts #-}

-- Defines a evaluation function for expressions
-- which implements a call-by-value big-step semantics

-- Small step semantics for guarded HOPFL
module Semantics.Evaluation where

import Control.Lens (set, view)
import Control.Lens.Setter (over)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExcept)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT))
import Data.Bifunctor (Bifunctor (second))
import Data.HashMap.Lazy as HM (HashMap, fromList, lookup)
import Debug.Trace (trace)
import Preprocess.AnnotateVars (annotateVars)
import Preprocess.Builtins (builtinsRaw)
import Semantics.Sampling (normalDist, randDist)
import Semantics.Substitution (
    recName,
    substList,
    substListCumulative,
    substitute,
 )
import Semantics.Tools (
    EvalContext (EvalContext),
    EvalMonad,
    ProbilityContext (ProbilityContext),
    evalAExp,
    evalAExp1,
    evalBExp,
    evalBExp1,
    evalDensity,
    evalDepth,
    evalEnv,
    evalMonad,
    evalRelop,
    evalVerbosity,
    forceEval,
    match2,
    mkEnv,
    performDraw,
    printEnv,
    randomDraws,
 )
import Semantics.Values (
    Value (
        VBox,
        VFalse,
        VIn,
        VInL,
        VInR,
        VNext,
        VPair,
        VSingle,
        VThunk,
        VTrue,
        VVal
    ),
    toExp,
 )
import qualified Syntax.Exp.Abs as Raw
import Syntax.Expression (Environment (Env), Exp (..), Ident (Ident))
import Syntax.Number (Number (Fract), numDiv, numMod, numPow)
import Syntax.Parse (parseExp)
import Tools.Treeify (treeTerm, treeValue)
import Tools.VerbPrint (putStrV)

builtins :: HM.HashMap String Exp
builtins = fromList $ map (second (annotateVars . parseExp)) builtinsRaw

-- Evaluates a program given a maximum eval depth, and environment
-- To be called from Main.hs
evaluate :: Int -> Exp -> Integer -> [Double] -> Raw.Environment -> IO ()
evaluate v prog n s env = do
    putStrV v ("Evaluating...\n- with random draws: " ++ show s)
    putStrV v ("- using the environment: " ++ printEnv (mkEnv env))
    putStrV v ("- up to depth: " ++ show n ++ "\n")

    let toEval = evalMonad (eval' prog)
    let r1 = runReaderT toEval $ EvalContext v (mkEnv env) n
    let r2 = runStateT r1 $ ProbilityContext s 1.0
    let r3 = runExcept r2 -- Run except to catch errors
    case r3 of
        Left s -> putStrLn $ "Evaluation failed:\n" ++ s
        Right (s, p_ctx) ->
            putStrLn $
                "Result (density = "
                    ++ show (view evalDensity p_ctx)
                    ++ ", remainings draws = "
                    ++ show (view randomDraws p_ctx)
                    ++ "):\n"
                    ++ treeValue s

-- Evaluation function:
-- Takes an AST and calculates the result of the program using big step semantics
eval :: Exp -> EvalMonad Value
-- Variables
eval exp@(Var (Ident v i r)) = do
    var <- asks (HM.lookup v . view evalEnv)
    let builtin = HM.lookup v builtins
    case (var, builtin) of
        -- Local variable take precedence over builtins
        (Just e, _) -> eval' e
        (Nothing, Just e) -> eval' e
        (Nothing, Nothing) ->
            throwError $
                "Undefined free variable: "
                    ++ show v
                    ++ " [id="
                    ++ show i
                    ++ "; depth="
                    ++ show r
                    ++ "]"

-- LetIn has a special type of substitution
eval exp@(LetIn (Env a) e) = eval' $ substListCumulative e a

-- Later modality: do no allow calculation past "depth" nexts
eval exp@(Next e) = do asks (view evalDepth) >>= go
  where
    go 0 = return $ VNext e
    go _ = local (over evalDepth $ subtract 1) (VNext . toExp <$> eval' e)

-- Put into fixpoint
eval exp@(In e) = return $ VIn e
-- Extract from fixpoint
eval exp@(Out e) = eval' e >>= go
  where
    go (VIn v) = eval' v
    go other =
        throwError $
            " Out on non-In:\n"
                ++ treeTerm exp
                ++ "\nOut value:\n"
                ++ show other

-- Function application
eval exp@(App e1 e2) = match2 eval' e1 e2 >>= go
  where
    go (VThunk (Abstr x e), r2) = eval' $ substitute e x $ toExp r2
    go _ = throwError $ " Application on non-function:\n" ++ treeTerm exp

-- Delayed function application
eval exp@(DApp e1 e2) = match2 eval' e1 e2 >>= go
  where
    go (VNext t, VNext s) = do
        trace ("\nt: " ++ treeTerm t ++ "\ns: " ++ treeTerm s) $
            eval' $
                Next $
                    App t s
    go (x, y) =
        throwError $
            "Invalid arguments to DApp:\n"
                ++ treeTerm exp
                ++ "\nArguments:\n- "
                ++ show x
                ++ "\n- "
                ++ show y

-- Pair creation
eval exp@(Pair e1 e2) = return $ VPair e1 e2
-- First projection
eval exp@(Fst e) = eval' e >>= go
  where
    go (VPair v1 v2) = eval' v1
    go err = throwError $ "Took fst of non-pair:\n" ++ treeValue err

-- Second projection
eval exp@(Snd e) = eval' e >>= go
  where
    go (VPair v1 v2) = eval' v2
    go err = throwError $ "Took snd of non-pair:\n" ++ treeValue err

-- Normal distribtion sampling
eval exp@(Norm e) = eval' e >>= go
  where
    go (VPair e1 e2) = match2 eval' e1 e2 >>= go'
    go _ = throwError $ "Normal argument not a pair of reals: \n" ++ treeTerm exp
    go' (VVal (Fract m), VVal (Fract v)) = performDraw normalDist [m, v]
    go' err = throwError $ "Normal pair does not contain reals: " ++ show err

-- Random uniform distrubition sampling
eval Rand = performDraw randDist []
-- Evaluate into values
eval exp@(Force e) = eval' e >>= forceEval eval'
-- If then else
eval exp@(Ite b e1 e2) = eval' b >>= go
  where
    go VTrue = eval' e1
    go VFalse = eval' e2
    go _ = throwError $ "If with non boolean condition:\n" ++ treeTerm exp

-- Coproduct injection
eval exp@(InL e) = return $ VInL e
eval exp@(InR e) = return $ VInR e
-- Matching coproducts
eval exp@(Match e x e1 y e2) = eval' e >>= go
  where
    go (VInL l) = eval' $ substitute e1 x l
    go (VInR r) = eval' $ substitute e2 y r
    -- go (VInL l) = eval' l >>= eval' . /substitute e1 x . toExp
    -- go (VInR r) = eval' r >>= eval' . substitute e2 y . toExp
    go _ = throwError $ "Match on non-coproduct:\n" ++ treeTerm exp

-- Function abstraction
eval exp@(Abstr x e) = return $ VThunk exp
-- Recursion
eval exp@(Rec x e) = eval' $ substitute e x (Next $ recName exp)
-- Prev: next inverse
-- Empty substitution list, simply remove the next
eval exp@(Prev (Env []) e) = eval' e >>= go
  where
    go (VNext e) = eval' e
    go _ = throwError $ "Took prev of non-next:\n" ++ treeTerm exp

-- Non empty list, perform substitutions
eval exp@(Prev (Env l) e) = eval' $ Prev (Env []) $ substList e l
-- (Un)Boxing
eval exp@(Box l e) = return $ VBox l e
eval exp@(Unbox e) = eval' e >>= go
  where
    go (VBox (Env l) e1) = eval' $ substList e1 l
    go _ = throwError $ "Unbox on non-box:\n" ++ treeTerm exp
eval exp = case exp of
    -- Instant values
    Val v -> return $ VVal v
    BTrue -> return VTrue
    BFalse -> return VFalse
    -- Arithmetic operators
    Min e -> evalAExp1 eval' negate e
    Pow e1 e2 -> evalAExp eval' e1 numPow e2
    Div e1 e2 -> evalAExp eval' e1 numDiv e2
    Mod e1 e2 -> evalAExp eval' e1 numMod e2
    Add e1 e2 -> do
        e1' <- eval e1
        e2' <- eval e2
        r <- evalAExp eval' e1 (+) e2
        trace (
            "Adding: " ++ show e1 ++ "(>" ++ show e1' ++ ")" ++ " + "
            ++ show e1 ++ "(" ++ show e2' ++ ") =" ++ show r) $
            evalAExp eval' e1 (+) e2
    Sub e1 e2 -> evalAExp eval' e1 (-) e2
    Mul e1 e2 -> evalAExp eval' e1 (*) e2
    -- Logic operators
    Not e -> evalBExp1 eval' not e
    And e1 e2 -> evalBExp eval' e1 (&&) e2
    Or e1 e2 -> evalBExp eval' e1 (||) e2
    -- Relative operators
    Eq e1 e2 -> evalRelop eval' e1 (==) e2
    Lt e1 e2 -> evalRelop eval' e1 (<) e2
    Gt e1 e2 -> evalRelop eval' e1 (>) e2
    Leq e1 e2 -> evalRelop eval' e1 (<=) e2
    Geq e1 e2 -> evalRelop eval' e1 (>=) e2
    -- Singleton term
    Single -> return VSingle

-- Wrapper around eval that corecurses with it to be able to print all
-- evaluation steps
eval' :: Exp -> EvalMonad Value
eval' e = do
    v <- asks $ view evalVerbosity
    if v >= 2
        then do
            result <- eval e
            trace ("evalExp(\n" ++ treeTerm e ++ ")\n= " ++ treeValue result) $ eval e
        -- trace ("evalExp(\n" ++ show e ++ "\n)") $ eval e
        else
            eval e
