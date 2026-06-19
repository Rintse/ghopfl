-- Defines some substitution functions needed for evaluating
-- recursion, function application, unboxing, previous and match statements
{-# LANGUAGE TypeFamilies #-}

module Semantics.Substitution where

import Syntax.Expression
import Syntax.Number

import Control.Monad.Reader (Reader, ask, asks, local, runReader)
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.List (inits)
import Debug.Trace (trace)

-- Increases the recursion depth of an idenitifier by 1
incDepth :: Ident -> Ident
incDepth (Ident x id d) = Ident x id (d + 1)

-- Renames for uniqueness of substituted values in recursion
-- Used in Prev and Box
recNameL :: Environment -> Environment
recNameL (Env l) = Env $ Prelude.map rec1 l
  where
    rec1 (Assign x t) = Assign (incDepth x) (recName t)

-- Renames all variables in body of rec statement to avoid
-- faulty application substitution in folded out rec terms
recName :: Exp -> Exp
recName = ana go
  where
    go (Var (Ident x 0 d)) = VarF $ Ident x 0 d -- Free variable
    go (Var v) = VarF $ incDepth v -- Not free, increment
    go (Prev l e) = PrevF (recNameL l) e
    go (Box l e) = BoxF (recNameL l) e
    go (Abstr x e) = AbstrF (incDepth x) e
    go (Rec x e) = RecF (incDepth x) e
    go (Match e x l y r) = MatchF e (incDepth x) l (incDepth y) r
    go other = project other

-- Performs substitution
-- Variables are the same if they have the same name, id and depth
doSub :: Ident -> (Ident, Exp) -> Exp
doSub v@(Ident x idx dx) (Ident y idy dy, s) =
    if x == y && idx == idy && dx == dy then s else Var v

-- Substitutes in substitution lists.
-- Used in Prev and Box
substL :: Environment -> Reader (Ident, Exp) Environment
substL (Env l) = Env <$> mapM (\(Assign x t) -> Assign x <$> subst t) l

-- Substitutes, in exp, x for s
subst :: Exp -> Reader (Ident, Exp) Exp
subst = apoM go
  where
    go (Var x) = asks (fmap Left . project . doSub x)
    go (Prev l e) = PrevF <$> substL l <*> return (Right e)
    go (Box l e) = BoxF <$> substL l <*> return (Right e)
    go other = return $ Right <$> project other

-- Substitutes in exp, s for x
substitute :: Exp -> Ident -> Exp -> Exp
substitute exp x s = runReader (subst exp) (x, s)

-- Perform substitution for a list of subs
substList :: Exp -> [Assignment] -> Exp
substList = Prelude.foldl (\e (Assign x t) -> substitute e x t)

-- Performs substitution on a list of assignments, substituting the k-th
-- assignment in all the following assignments [k+1:n]:
-- Example: substListCumulative [x <- a; y <- b; z <- c] e
--   1. tmp1 = substList a []
--   2. tmp2 = substList b [x <- tmp1]
--   3. tmp3 = substList c [x <- tmp1; y <- tmp2]
--   4. substList e [x <- tmp1; y <- tmp2; z <- tmp3]
substListCumulative :: Exp -> [Assignment] -> Exp
substListCumulative e l = trace ("subbing in " ++ show e ++ " : " ++ show subbedList)substList e subbedList
  where
    subbedList = doSub l []
    doSub :: [Assignment] -> [Assignment] -> [Assignment]
    doSub [] acc = reverse acc
    doSub (Assign x a : rest) acc = doSub rest (newSub : acc)
      where
        subbedA = substList a acc
        newSub = Assign x subbedA
