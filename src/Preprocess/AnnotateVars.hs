-- Defines a custom datastructure to contain the program, which is
-- nearly identical to the raw parsed data, except that identifiers
-- are annotated with a unique id to aid in substitution. Also defines a
-- function that transforms raw expressions into their annotated versions
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Preprocess.AnnotateVars where

import qualified Syntax.Exp.Abs as Raw
import Syntax.Expression
import Syntax.Number
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import qualified Data.Map as HM
import Data.List.Index
import qualified Data.Set as Set
import Debug.Trace
import Preprocess.Definitions (runDef)
import qualified Control.Lens as Lens
import Control.Lens (over, view, set)
import Preprocess.Builtins (builtins)

-- State environment contains a counter and a hashmap in which the values
-- track all the ids that we are currently substituting the key for.
-- The last element in the sequence is the id to substitute the key for.
type IdMap = HM.Map String Int

newtype TransformContext = TransformContext
    { _varIds :: IdMap
    }
Lens.makeLenses ''TransformContext

newtype IdMonad a = IdMonad
    { runId :: Reader TransformContext a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader TransformContext
        )

-- Increments counter and pushes new substitute for x onto m[x]
-- Note: insertWith calls (++) with argument order: (++) new old
incVar :: Raw.Ident -> IdMap -> IdMap
incVar (Raw.Ident x) = HM.insertWith (+) x 1

bindList :: [Raw.Assignment] -> IdMap -> IdMap
bindList l m = do
    let vars = map go l where go (Raw.Assign x _ _) = x
    foldl (flip incVar) m vars

-- Rename an individual substitution
varAssign :: Raw.Assignment -> IdMonad Assignment
varAssign (Raw.Assign x _ t) = do
    ident <- asks (getSub x . incVar x . view varIds)
    Assign ident <$> transform t

-- Gets the latest substitute for x from m[x] (returns x if none are found)
getSub :: Raw.Ident -> IdMap -> Ident
getSub (Raw.Ident x) m = do
    case (m HM.!? x, builtins HM.!? x) of
        (Just i, _) -> Ident x i 0
        (Nothing, Just _) -> Ident x 0 0
        (Nothing, Nothing) -> error $ "Free variable?: " ++ x

-- Gets all free variables in an assignment list
getFreesL :: [Assignment] -> Set.Set Ident
getFreesL = foldr (Set.union . (\(Assign x t) -> getFrees t)) Set.empty

-- Gets all free variables in an expression
getFrees :: Exp -> Set.Set Ident
getFrees = cata go where
    go (VarF id@(Ident x d _)) = if d == 0 then Set.singleton id else Set.empty
    go (ValF _) = Set.empty
    go (PrevF (Env l) e) = Set.union e $ getFreesL l
    go (BoxF (Env l) e) = Set.union e $ getFreesL l
    go fFree = foldr Set.union Set.empty fFree

-- Transforms an identifier into an identity substitution for that identifier
idSubst :: Ident -> Raw.Assignment
idSubst (Ident x _ _) = Raw.Assign (Raw.Ident x) (Raw.TSub "")
    (Raw.Var $ Raw.Ident x)

-- Returns the identity substitution list for all free variables
-- in term e. Used in boxF and prevF
freeList :: Raw.Exp -> Raw.Environment
freeList e = Raw.Env $ map idSubst $ Set.toList $ getFrees (annotateVars e)

-- TODO check for faulty programs?
-- Transforms the raw syntax tree into a version where the
-- idenfiers are made unique with an id and recursion depth tag.
transform :: Raw.Exp -> IdMonad Exp
transform exp = case exp of
    -- Annotate variables with a unique (per variable name) ID
    Raw.Var v -> asks (Var . getSub v . view varIds)
    -- Integers and doubles into one overarching number type
    Raw.DVal v -> return $ Val $ Fract v
    Raw.IVal v -> return $ Val $ Whole v
    -- Simple 1-to-1 correspondence.
    Raw.Single t -> return Single
    Raw.BTrue -> return BTrue
    Raw.BFalse -> return BFalse
    Raw.Unbox e -> fmap Unbox (transform e)
    Raw.Force e -> fmap Force (transform e)
    Raw.Norm e -> fmap Norm (transform e)
    Raw.Rand -> return Rand
    Raw.Next e -> fmap Next (transform e)
    Raw.Out e -> fmap Out (transform e)
    Raw.Fst e -> fmap Fst (transform e)
    Raw.Snd e -> fmap Snd (transform e)
    Raw.InL e -> fmap InL (transform e)
    Raw.InR e -> fmap InR (transform e)
    Raw.Not _ e -> fmap Not (transform e)
    Raw.Min e -> fmap Min (transform e)
    Raw.In e -> fmap In (transform e)
    Raw.LApp e1 _ e2 -> liftA2 DApp (transform e1) (transform e2)
    Raw.Pair e1 e2 -> liftA2 Pair (transform e1) (transform e2)
    Raw.Leq e1 o e2 -> liftA2 Leq (transform e1) (transform e2)
    Raw.Geq e1 o e2 -> liftA2 Geq (transform e1) (transform e2)
    Raw.App e1 e2 -> liftA2 App (transform e1) (transform e2)
    Raw.Add e1 e2 -> liftA2 Add (transform e1) (transform e2)
    Raw.Sub e1 e2 -> liftA2 Sub (transform e1) (transform e2)
    Raw.Mul e1 e2 -> liftA2 Mul (transform e1) (transform e2)
    Raw.Mod e1 e2 -> liftA2 Mod (transform e1) (transform e2)
    Raw.Pow e1 e2 -> liftA2 Pow (transform e1) (transform e2)
    Raw.Div e1 e2 -> liftA2 Div (transform e1) (transform e2)
    Raw.And e1 o e2 -> liftA2 And (transform e1) (transform e2)
    Raw.Or e1 o e2 -> liftA2 Or (transform e1) (transform e2)
    Raw.Eq e1 e2 -> liftA2 Eq (transform e1) (transform e2)
    Raw.Lt e1 e2 -> liftA2 Lt (transform e1) (transform e2)
    Raw.Gt e1 e2 -> liftA2 Gt (transform e1) (transform e2)
    Raw.Ite b e1 e2 -> liftA3 Ite (transform b) (transform e1) (transform e2)
    -- Syntactic sugar
    Raw.PrevE e -> transform $ Raw.Prev (Raw.Env []) e
    Raw.BoxI e -> transform $ Raw.Box (freeList e) e
    Raw.PrevI e -> transform $ Raw.Prev (freeList e) e
    -- WARNING: Here be binders
    Raw.LetIn (Raw.Env l) e -> do
        rl <- mapM varAssign l
        re <- local (over varIds $ bindList l) $ transform e
        return $ LetIn (Env rl) re
    Raw.Box (Raw.Env l) e -> do
        rl <- mapM varAssign l
        re <- local (over varIds $ bindList l) $ transform e
        return $ Box (Env rl) re
    Raw.Prev (Raw.Env l) e -> do
        rl <- mapM varAssign l
        re <- local (over varIds $ bindList l) $ transform e
        return $ Prev (Env rl) re
    Raw.Match e x _ l y _ r -> do
        re <- transform e -- Nothing binds e
        rx <- asks (getSub x . incVar x . view varIds)
        rl <- local (over varIds $ incVar x) $ transform l
        ry <- asks (getSub y . incVar y . view varIds)
        rr <- local (over varIds $ incVar y) $ transform r
        return $ Match re rx rl ry rr
    Raw.Abstr _ x e -> do
        rx <- asks (getSub x . incVar x . view varIds)
        re <- local (over varIds $ incVar x) $ transform e
        return $ Abstr rx re
    Raw.Rec f e -> do
        rf <- asks (getSub f . incVar f . view varIds)
        re <- local (over varIds $ incVar f) $ transform e
        return $ Rec rf re

-- Translate a raw tree into the id tree with annotated identifiers
annotateVars :: Raw.Exp -> Exp
annotateVars e = do
    let ctx = TransformContext HM.empty
    runReader (runId (transform e)) ctx
