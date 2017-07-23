{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}
{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import           Olifant.Core

import qualified Data.Map.Strict as Map
import           Prelude         (String)
import           Protolude       hiding (cast, panic)

-- Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- Env maps textual representation to a refined reference
type Env = Map Text Ref

-- | Top level API of the module
compile :: [Calculus] -> Either Error Progn
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
cast :: [Calculus] -> Compiler Progn
cast cs =
    case unsnoc cs of
        Just (decls, main) -> Progn <$> mapM top decls <*> inner main
        Nothing            -> serr $ toS (show cs :: String)
  where
    -- | Translate a top level expression
    top :: Calculus -> Compiler Bind
    top (CLet var val) = inner val >>= return . Bind (Ref var TUnit Global)

    top l = serr $ "Expected let expression, got " <> show l <> " instead"
    -- | Translate a nested expression
    inner :: Calculus -> Compiler Expr
    inner (CVar a) = return $ Var (Ref a TUnit Local)
    inner (CNumber n) = return $ Number n
    inner (CBool k) = return $ Bool k
    inner (CApp fn' args') = do
        fn <- inner fn'
        args <- mapM inner args'
        return $ App TUnit fn args
    inner (CLam args' body') = do
        let args = [Ref n t Local | (t, n) <- args']
        body <- inner body'
        return $ Lam TUnit args body
    inner (CLet _ _) =
        case cs of
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
    inner (Var ref) = do
        ref' <- fetch $ rname ref
        maybe e (return . Var) ref'
      where
        e = throwError $ UndefinedError ref
    inner l@Number {} = return l
    inner l@Bool {} = return l
    inner (App _ fn' args') = do
        fn <- inner fn'
        args <- mapM inner args'
        when (arity (ty fn) /= length args) $ throwError TyError
        case apply (ty fn) (map ty args) of
            Just t  -> return $ App t fn args
            Nothing -> throwError TyError
    inner (Lam _ args body') = do
        mapM_ (\arg -> extend (rname arg) arg) args
        body <- inner body'
        let t = unapply (ty body) (map rty args)
        return $ Lam t args body

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
    inner (Var ref) = maybeToList <$> fetch (rname ref)
    inner Bool {} = return []
    inner Number {} = return []
    inner (App _t fn args) = return (++) <*> inner fn <*> concatMapM inner args
    inner (Lam _fn args body) = do
        mapM_ (\arg -> modify (Map.insert (rname arg) arg)) args
        inner body

-- * Aliases to errors raised by the compiler
serr :: Text -> Compiler a
serr = throwError . SyntaxError

panic :: Text -> Compiler a
panic = throwError . Panic
