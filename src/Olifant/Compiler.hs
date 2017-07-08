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

-- Env maps textual representation to a refined reference
type Env = Map Text Ref

-- | Top level API of the module
compile :: [C.Calculus] -> Either Error Progn
compile ls = evalM (cast ls >>= rename >>= infer >>= verify) mempty

-- | Get type of a reference from the symbol table
fetch :: Text -> Compiler (Maybe Ref)
fetch t = Map.lookup t <$> get

-- | Extend the symbol table with a ref
extend :: Text -> Ref -> Compiler ()
extend t r = modify $ Map.insert t r

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
    top (C.Let var val) = inner val >>= return . Bind (Ref var TUnit Global)
    top l = serr $ "Expected let expression, got " <> show l <> " instead"

    -- | Translate a nested expression
    inner :: C.Calculus -> Compiler Expr
    inner (C.Var a)      = return $ Var (Ref a TUnit Local)
    inner (C.Number n)   = return $ Lit (LNumber n)
    inner (C.Bool k)     = return $ Lit (LBool k)
    inner (C.App fn arg) = App TUnit <$> inner fn <*> inner arg
    inner (C.Lam n t b)  = inner b >>= \x -> return $ Lam TUnit (Ref n t Local) x
    inner (C.Let _ _)    = case cs of
      [_] -> serr "Body can't be just a let expression"
      _   -> serr "Nested let expression"

-- Infer types
--
infer :: Progn -> Compiler Progn
infer (Progn decls main) = Progn <$> mapM top decls <*> inner main
  where
    top :: Bind -> Compiler Bind
    top (Bind (Ref n _ _) core) = do
      core' <- inner core
      let ref = Ref n (ty core') Global
      extend n ref
      return $ Bind ref core'

    inner :: Expr -> Compiler Expr
    inner (Var ref)    = do
        ref' <- fetch $ rname ref
        maybe e (return . Var) ref'
      where
        e = throwError $ UndefinedError ref

    inner l@Lit{} = return l

    inner (App _ fn arg) = do
        fn' <- inner fn
        arg' <- inner arg
        case curry (ty arg') (ty fn') of
            Just t  -> return $ App t fn' arg'
            Nothing -> throwError $ TyError arg (argT $ ty fn') (ty arg')

    inner (Lam _ arg body) = do
        extend (rname arg) arg
        body' <- inner body
        let t = uncurry (rty arg) (ty body')
        return $ Lam t arg body'

-- | Rename expression to avoid shadowing
rename :: Progn -> Compiler Progn
rename = return

-- | Verify the core
--
-- 1. Ensure redundant types match
verify :: Progn -> Compiler Progn
verify = return

-- | Find  variables; typed or untyped program
free :: Progn -> Compiler [Ref]
free (Progn decls main) = concat <$> liftA2 (:) (inner main) (mapM top decls)
  where
    top :: Bind -> Compiler [Ref]
    top (Bind ref val) = put (Map.singleton (rname ref) ref) >> inner val

    inner :: Expr -> Compiler [Ref]
    inner (Var  ref) = maybeToList <$> fetch (rname ref)
    inner Lit{} = return []
    inner (App _t fn exp') = return (++) <*> inner fn <*> inner exp'
    inner (Lam _fn arg body) = modify (Map.insert (rname arg) arg) >> inner body

-- * Aliases to errors raised by the compiler

serr :: Text -> Compiler a
serr = throwError . SyntaxError

panic :: Text -> Compiler a
panic = throwError . Panic
