{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}

{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import qualified Olifant.Calculus as C
import           Olifant.Core

import qualified Data.Map.Strict as Map
import           Prelude         (String)
import           Protolude       hiding (cast, curry, panic, uncurry)

-- Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- Env is a simple list of references
type Env = Map Ref Tipe

-- | Top level API of the module
compile :: [C.Calculus] -> Either Error Progn
compile ls = evalM (cast ls >>= rename >>= infer >>= verify) Map.empty

-- | Get type of a reference from the symbol table
fetch :: Ref -> Compiler (Maybe Tipe)
fetch r = Map.lookup r <$> get

-- | Extend the symbol table with a ref
extend :: Tipe -> Ref -> Compiler ()
extend t r = modify $ Map.insert r t

-- | Transform calculus into untyped core
--
-- Input program should be a series of let bindings and one expression in the
-- end. Validate structure of the program and fill TUnit types
cast :: [C.Calculus] -> Compiler Progn
cast cs = case unsnoc cs of
    Just (decls, main) -> Progn <$> mapM top decls <*> inner main
    Nothing            -> serr $ toS (show cs :: String)
  where
    -- | Translate a top level expression
    top :: C.Calculus -> Compiler Bind
    top (C.Let var val) = inner val >>= return . Bind (Ref var)
    top l = serr $ "Expected let expression, got " <> show l <> " instead"

    -- | Translate a nested expression
    inner :: C.Calculus -> Compiler Expr
    inner (C.Var a)      = return $ Var TUnit (Ref a)
    inner (C.Number n)   = return $ Lit TInt (LNumber n)
    inner (C.Bool k)     = return $ Lit TBool (LBool k)
    inner (C.App fn arg) = App TUnit <$> inner fn <*> inner arg
    -- @t@ is the type of the argument here, not the function. This will be
    -- fixed during inference pass.
    inner (C.Lam n t b)  = inner b >>= \x -> return $ Lam t (Ref n) x
    inner (C.Let _ _)    = case cs of
      [_] -> serr "Body can't be just a let expression"
      _   -> throwError $ SyntaxError "Nested let expression"

-- Infer types
--
infer :: Progn -> Compiler Progn
infer (Progn decls main) = Progn <$> mapM top decls <*> inner main
  where
    top :: Bind -> Compiler Bind
    top (Bind ref core)  = do
        core' <- inner core
        extend (tipe core') ref
        return $ Bind ref core'

    inner :: Expr -> Compiler Expr
    inner (Var _ a)      = fetch a >>= maybe e (return . flip Var a)
      where
        e = throwError $ UndefinedError a

    inner l@Lit{} = return l

    inner (App _ fn arg) = do
      fn' <- inner fn
      arg' <- inner arg
      let result = curry (tipe arg') (tipe fn')
      case result of
        Just t  -> return $ App t fn' arg'
        Nothing -> throwError $ TipeError fn (argT $ tipe fn') (tipe fn')

    -- @t@ is the type of the argument, not the entire function because that's
    -- how annotations work in simply typed λ calculus works. @t'@ is the type
    -- of the function.
    inner (Lam t arg body) = do
      extend t arg
      body' <- inner body
      let t' = uncurry t (tipe body')
      return $ Lam t' arg body'

-- | Rename expression to avoid shadowing
rename :: Progn -> Compiler Progn
rename = return

-- | Type check the core
verify :: Progn -> Compiler Progn
verify = return

-- | Find  variables; typed or untyped program
free :: Progn -> Compiler [Ref]
free (Progn decls main) = concat <$> liftA2 (:) (inner main) (mapM top decls)
  where
    top :: Bind -> Compiler [Ref]
    top (Bind ref val) = put (Map.singleton ref TUnit) >> inner val

    inner :: Expr -> Compiler [Ref]
    inner  (Var _ x) = get >>= \acc -> return $ if Map.member x acc then [] else [x]
    inner  Lit{} = return []
    inner (App _t fn exp') = return (++) <*> inner fn <*> inner exp'
    inner (Lam _fn arg body) = modify (Map.insert arg TUnit) >> inner body

-- * Aliases to errors raised by the compiler

serr :: Text -> Compiler a
serr = throwError . SyntaxError

panic :: Text -> Compiler a
panic = throwError . Panic
