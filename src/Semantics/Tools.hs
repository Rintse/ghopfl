-- Defines some helper functions for the Evaluation module
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Semantics.Tools where

import Control.Exception.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.Functor.Foldable.TH
import Data.HashMap.Lazy as HM
import Debug.Trace
import Preprocess.AnnotateVars
import Semantics.Sampling
import Semantics.Values
import Syntax.Expression
import Syntax.Number
import Syntax.Exp.ErrM
import qualified Syntax.Exp.Abs as Raw
import Control.Lens

-- Environment as hashmap from names to values
type Env = HashMap String Exp

data EvalContext = EvalContext
    { _evalVerbosity :: Int
    , _evalEnv :: Env
    , _evalDepth :: Integer
    } deriving ()
makeLenses ''EvalContext

data ProbilityContext = ProbilityContext
    { _randomDraws :: [Double]
    , _evalDensity :: Double
    }
makeLenses ''ProbilityContext

newtype EvalMonad a = EvalMona
    { evalMonad :: ReaderT EvalContext (StateT ProbilityContext (Except String)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader EvalContext
        , MonadState ProbilityContext
        , MonadError String
        )

-- Transform the environment AST into a hashmap
mkEnv :: Raw.Environment -> Env
mkEnv (Raw.Env e) = fromList $ fmap mkAssign e
  where
    mkAssign (Raw.Assign (Raw.Ident x) _ exp) =
        (x, annotateVars exp)

printEnv :: Env -> String
printEnv m =
    show $
        Prelude.map
            (\x -> fst x ++ "=" ++ show (snd x))
            (toList m)

-- Evaluates 2 arguments and pairs them to allow for easy pattern matching
match2 :: (Exp -> EvalMonad Value) -> Exp -> Exp -> EvalMonad (Value, Value)
match2 f e1 e2 = (,) <$> f e1 <*> f e2

match3 :: (Exp -> EvalMonad Value) -> Exp -> Exp -> Exp -> EvalMonad (Value, Value, Value)
match3 f e1 e2 e3 = (,,) <$> f e1 <*> f e2 <*> f e3

-- Performs a random draw and updates the state monad
performDraw :: Distribution -> [Double] -> EvalMonad Value
performDraw dist params = do
    draws <- gets $ view randomDraws
    case draws of
        (c : rest) ->
            let d = pdf dist params c
            in if
                | isNaN d ->
                    throwError $
                        "PDF " ++ show (name dist)
                        ++ " not defined for value "
                        ++ show d
                | (== 0) d ->
                    throwError $
                        "Impossible draw for "
                            ++ show (name dist)
                            ++ " with parameters"
                            ++ show params
                            ++ ": "
                            ++ show c
                            ++ "\nRemaining draws: "
                            ++ show rest
                | otherwise -> do
                    modify (over evalDensity (*d) . set randomDraws rest)
                    return $ VVal $ Fract c
        _ -> throwError "Draws list too small"

-- Helper functions to evaluate boolean and aritmetic expresions
-- Evaluates a binary arithmetic operation
evalAExp ::
    (Exp -> EvalMonad Value) ->
    Exp ->
    (Number -> Number -> Number) ->
    Exp ->
    EvalMonad Value
evalAExp f e1 op e2 =
    match2 f e1 e2 >>= \case
        (VVal v1, VVal v2) -> return $ VVal $ op v1 v2
        other -> throwError $ "Non-real args to arithmetic operator:\n" ++ show other

evalAExp1 ::
    (Exp -> EvalMonad Value) ->
    ( Number ->
      Number
    ) ->
    Exp ->
    EvalMonad Value
evalAExp1 f op e =
    f e >>= \case
        (VVal v) -> return $ VVal $ op v
        other -> throwError $ "Non-real arg to arithmetic operator:\n" ++ show other

-- Evaluates a binary boolean operation
evalBExp ::
    (Exp -> EvalMonad Value) ->
    Exp ->
    (Bool -> Bool -> Bool) ->
    Exp ->
    EvalMonad Value
evalBExp f e1 op e2 =
    match2 f e1 e2 >>= \case
        (VBVal b1, VBVal b2) -> return $ VBVal $ op b1 b2
        other -> throwError $ "Non-bool args to bool operator:\n" ++ show other

-- Evaluates a unary boolean operation
evalBExp1 ::
    (Exp -> EvalMonad Value) ->
    (Bool -> Bool) ->
    Exp ->
    EvalMonad Value
evalBExp1 f op e =
    f e >>= \case
        VBVal b -> return $ VBVal $ op b
        other -> throwError $ "Non-bool args to bool operator:\n" ++ show other

-- Evaluates a relative operator
evalRelop ::
    (Exp -> EvalMonad Value) ->
    Exp ->
    (Number -> Number -> Bool) ->
    Exp ->
    EvalMonad Value
evalRelop f e1 op e2 =
    match2 f e1 e2 >>= \case
        (VVal v1, VVal v2) -> return $ VBVal $ op v1 v2
        other -> throwError $ "Non-real args to relative operator:\n" ++ show other

-- Evluates everything underneath certain values to make it readable
-- TODO: shouldn't the semantics dictate this?
forceEval :: (Exp -> EvalMonad Value) -> Value -> EvalMonad Value
forceEval f = \case
    -- Still refuse to go underneath too many nexts
    VNext e -> do
        depth <- asks view evalDepth
        if depth == 0
            then return $ VUNext e
            else local 
                (over evalDepth $ subtract 1) 
                (VENext <$> (f e >>= forceEval f))
    VIn e -> VEIn <$> (f e >>= forceEval f)
    VInL e -> VEInL <$> (f e >>= forceEval f)
    VInR e -> VEInR <$> (f e >>= forceEval f)
    VBox l e -> VEBox <$> (f e >>= forceEval f)
    VPair e1 e2 -> VEPair <$> (f e1 >>= forceEval f) <*> (f e2 >>= forceEval f)
    other -> return other
