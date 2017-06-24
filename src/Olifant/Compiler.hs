{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}

{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import qualified Olifant.Calculus as C
import           Olifant.Core
import           Prelude          (String)
import           Protolude

-- Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- Env is a simple list of references
type Env = [Ref]

compile :: [C.Calculus] -> Either Error [Bind ()]
compile ls = evalM (translate ls) []

-- | Compile a series of Calculus expressions into untyped core bindings
--
-- Input program should be a series of let bindings and one expression in the
-- end.
--
translate :: [C.Calculus] -> Compiler [Bind ()]
translate [main] = t1 main >>= \m -> return [Main m]
translate (C.Let var val:xs) = do
  val' <- t1 val
  rest <- translate xs
  return $ Bind (Ref var) val': rest

translate x = throwError $ SyntaxError $ toS (show x :: String)

-- | Translate a single calculus expression into untyped core
--
-- This function is partial and should not be used directly.
--
t1 :: C.Calculus -> Compiler CoreUT
t1 (C.Var a)      = return $ Var unit $ Ref a
t1 (C.Number n)   = return $ Lit unit (LNumber n)
t1 (C.Bool b)     = return $ Lit unit (LBool b)
t1 (C.App fn arg) = App unit <$> t1 fn <*> t1 arg
t1 (C.Lam n b)    = Lam unit (Ref n) <$> t1 b
t1 (C.Let _ _)    = throwError $ SyntaxError "Invalid let expression "

-- | Find free variables in an expression; typed or untyped
free :: Expr a -> [Ref]
free core = free' core []
  where
    free' :: Expr a -> [Ref] -> [Ref]

    free' (Var _ x) acc          = if x `elem` acc then [] else [x]

    free' Lit{} _                = []

    free' (App _t f exp') acc    = free' f acc ++ free' exp' acc

    -- [todo] - Evaluate body with arg as well
    free' (Lam _fn arg body) acc = free' body (arg: acc)
