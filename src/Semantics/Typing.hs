{-# LANGUAGE LambdaCase #-}

module Semantics.Typing where

import Syntax.Expression
import Syntax.Types.Abs
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Applicative (liftA3)

-- https://en.wikipedia.org/wiki/Hindley–Milner_type_system#An_inference_algorithm

typeCheck :: Exp -> Except String Ty
typeCheck BTrue = return Bool
typeCheck BFalse = return Bool
typeCheck (Val _) = return Real
typeCheck (Pair a b) = liftA2 Prod (typeCheck a) (typeCheck b)
typeCheck (Norm a) = typeCheck a >>= \case
    Prod _ _ -> return Real
    other -> throwError $ "Norm term with " ++ show other ++ " as argument"

typeCheck (Ite c a b) = 
    liftA3 (,,) (typeCheck c) (typeCheck a) (typeCheck b) >>= \case
    (Bool, ta, tb) | ta == tb -> return ta
    (Bool, ta, tb) | ta /= tb -> 
        throwError $ "If and else bodies have differing types: " 
            ++ show ta ++ " != " ++ show tb
    other -> throwError $ "If condition with " ++ show other ++ " as argument"

typeCheck (InL a) = liftA2 Prod 
    (typeCheck a) $ return $ 
        TyForall (TForall "") 
        (Syntax.Types.Abs.Ident "a") 
        (Syntax.Types.Abs.Var $ Syntax.Types.Abs.Ident "a")

typeCheck (Fst a) = typeCheck a >>= \case
    Prod x _ -> return x
    other -> throwError $ "Fst on non-product type: " ++ show other

typeCheck (Snd a) = typeCheck a >>= \case
    Prod _ x -> return x
    other -> throwError $ "Snd on non-product type: " ++ show other
        

-- TODO
typeCheck (App a b) = do
    let ta = typeCheck a
    let tb = typeCheck b
    return Bool
