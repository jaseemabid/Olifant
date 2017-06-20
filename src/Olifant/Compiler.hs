{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}
module Olifant.Compiler where

import Protolude hiding (cast)

import Olifant.Core
import qualified Olifant.Calculus as C

-- | Quickly translate calculus into untyped core
--
-- This is a dumb stupid pass which is making me reconsider having the Calculus
-- type at all. Maybe I can just avoid Calculus type and parse into CoreUT
-- directly.
--
cast :: C.Calculus -> CoreUT
cast (C.Var a) = Var unit $ Ref a
cast (C.Number n) = Lit unit (LNumber n)
cast (C.Bool b) = Lit unit (LBool b)
cast (C.App fn arg') = App unit (cast fn) (cast arg')
cast (C.Lam n b) = Lam unit (Ref n) (cast b)
cast (C.Let _ _) = error "Cannot translate let expression to core"

-- |  Compile a series of Calculus expressions into untyped core bindings
--
-- Consider the simplest well formed calculus for now. Only top level functions
-- and variables. Last one is an expression which will be wrapped into a main.
rename :: [C.Calculus] -> [Bind ()]
rename = undefined

-- | Find free variables in an expression; typed or untyped
free :: Expr a -> [Ref]
free core = free' core []
  where
    free' :: Expr a -> [Ref] -> [Ref]

    free' (Var _ x) acc = if x `elem` acc then [] else [x]

    free' Lit{} _ = []

    free' (App _t f exp') acc = free' f acc ++ free' exp' acc

    -- [todo] - Evaluate body with arg as well
    free' (Lam _fn arg body) acc = free' body (arg: acc)
