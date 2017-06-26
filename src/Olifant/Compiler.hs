{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}

{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import qualified Olifant.Calculus as C
import           Olifant.Core
import           Prelude          (String)
import           Protolude hiding (uncurry)

-- Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- Env is a simple list of references
type Env = [Ref]

-- | Top level API of the module
compile :: [C.Calculus] -> Either Error [Bind Tipe]
compile ls = evalM (translate ls >>= typecheck) []

-- | Compile a series of Calculus expressions into untyped core bindings
--
-- Input program should be a series of let bindings and one expression in the
-- end. This is the first transformation by the compiler.
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

-- | Type check!
--
-- This is pretty stupid and naive. Implement Hindley-Milner soon
typecheck :: [Bind ()] -> Compiler [Bind Tipe]
typecheck  = mapM  f
  where
    f :: Bind () -> Compiler (Bind Tipe)
    f b@(Bind ref expr) = hm expr >>= \tipe -> return $ fmap (const tipe) b

    hm :: Expr () -> Compiler Tipe
    hm (Var () _ref) = err "No aliases yet"
    hm (Lit () (LNumber _)) = return TInt
    hm (Lit () (LBool _)) = return TBool
    hm (App () _e1 _e2)  = err "App needs lam before"
    -- Naively consider the type of argument to be i64
    hm (Lam () _ref body) = hm body >>= return . uncurry TInt

-- | Find free variables; typed or untyped core
free :: [Bind ()] -> Compiler [Ref]
free [] = return []
free (b:bs) = case b of
    Bind ref val -> put [ref] >> return (++) <*> f val <*> free bs
    Main val     -> return (++) <*> f val <*> free bs

  where
    f :: Expr a -> Compiler [Ref]
    f (Var _ x) = get >>= \acc -> return $ if x `elem` acc then [] else [x]
    f Lit{} = return []
    f (App _t fn exp') = return (++) <*> f fn <*> f exp'
    f (Lam _fn arg body) = modify (\s -> arg: s) >> f body

-- | Errors raised by the compiler
err :: Text -> Compiler a
err = throwError . TipeError
