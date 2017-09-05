{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import           Olifant.Core

import           Prelude (last)
import           Protolude hiding (Const, panic)
import qualified Data.Map.Strict as Map

-- | Env maps textual representation to a refined reference
type Env = Map Text Ref

-- | Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- * Exposed API

-- | Default environment
environment :: Env
environment = Map.fromList $ env1 <> env2
  where
    binops :: [Text]
    binops = ["sum", "sub", "mul", "div", "shr", "shl"]

    boolops :: [Text]
    boolops = ["lt", "gt", "eq"]

    env1 :: [(Text, Ref)]
    env1 = [(n, Ref n 0 (TInt :> TInt :> TInt) Extern) | n <- binops]

    env2 :: [(Text, Ref)]
    env2 = [(n, Ref n 0 (TInt :> TInt :> TBool) Extern) | n <- boolops]

-- | Top level API of the module
compile :: [Calculus] -> Either Error [Core]
compile ls = evalM (structure ls >>= infer >>= rename >>= verify) environment

-- * State Management

-- | Get type of a reference from the symbol table
fetch :: Text -> Compiler (Maybe Ref)
fetch t = Map.lookup t <$> get

-- | Extend the symbol table with a ref
extend :: Text -> Ref -> Compiler ()
extend t r = modify $ Map.insert t r

-- | Classic append
append :: [a] -> a -> [a]
append a b = a ++ [b]

-- * Type helpers

-- | Get the type of a core expression
ty :: Core -> Compiler Ty
ty (Lit (Number _)) = return TInt
ty (Lit (Bool _))   = return TBool
ty (Var ref)        = return $ rty ref
ty (Lam ref _ _)    = return $ rty ref
ty (App ref args)   = do
    -- Type check args
    args' <- mapM ty args

    --  Arity check
    when (arity (rty ref) /= length args) $
        throwError TyError {expr = App ref args}

    -- Type matching
    case apply (rty ref) args' of
        Just t  -> return t
        Nothing -> throwError TyError {expr = App ref args}

ty (Let ref _)      = return $ rty ref

-- | Return type of a type
retT :: Ty -> Ty
retT (_ :> tb) = retT tb
retT t         = t

-- | Argument of a type; the curried way
argT :: Ty -> Ty
argT (ta :> _) = ta
argT t         = t

-- | Arity of a type
arity :: Ty -> Int
arity t = length (flatT t) - 1

-- | Flatten a type
flatT :: Ty -> [Ty]
flatT (ta :> tb) = ta: flatT tb
flatT t          = [t]

-- [TODO] - Change type to Either Ty TyError
-- | Apply arguments to a function at the type level
apply :: Ty -> [Ty] -> Maybe Ty
apply ta [] = return ta
apply (ta :> tb) (tz:ts)
    | tz == ta = apply tb ts
    | otherwise = Nothing
apply _ _ = Nothing

-- | Make function type from the argument types & body type
unapply :: Ty -> [Ty] -> Ty
unapply = foldr (:>)

-- | Validate structure of the program
--
-- It should be a series of optional let bindings and one expression in the end
structure :: [Calculus] -> Compiler [Calculus]
structure = \case
    [ ]  -> serr "Expected a list of let bindings and a command"
    [x]  -> main x >>= \x' -> return [x']
    x:xs -> do
      d <- decl x
      r <- structure xs
      return $ d : r

  where
    -- Verify the declarations
    decl :: Calculus -> Compiler Calculus
    decl c@CLet{} = return c
    decl c@CLam{} = return c
    decl c = serr $ "Expected let expression, got " <> render c <> " instead"

    -- Verify the last expression
    main CLam{} = serr "Body can't be a function declaration"
    main CLet{} = serr "Body can't be a let binding"
    main calc   = return calc

-- | Transform calculus into core
--
-- This pass is clearly not in the spirit of nanopass compilers. It does a bit
-- too much, but that might be due to a very liberal `Calculus` and a reasonably
-- sane Core.
--
infer :: [Calculus] -> Compiler [Core]
infer = mapM emit
  where
    -- Translate a nested expression
    emit :: Calculus -> Compiler Core

    -- Literals are the same
    emit (CLit n) = return $ Lit n

    -- Calculus variable is just a text, Core variable is a `Ref`.
    -- [TODO] - Ensure types match here
    emit (CVar _ var) =
      fetch var >>= \case
        Just ref' -> return $ Var ref'
        Nothing   -> throwError $ UndefinedError var

    emit (CLam name args body) = do
        body' <- localized $ do
            gets (Map.union fenv) >>= put
            mapM emit body

        t' <- ty $ last body'

        -- Compute the type of the function from args and body
        let t = unapply t' (map rty rargs)

        -- trace ((show ("TRACE", ty body', t)) :: Text) $ return ()

        let fref = Ref {rname = name, ri = 0, rty = t, rscope = Global}

        extend name fref
        return $ Lam fref rargs body'
      where
        -- Make references out of all the args
        rargs :: [Ref]
        rargs = [Ref n 0 t Local | (t, n) <- args]

        -- Make a new environment for the body extending env with args
        fenv = Map.fromList [(rname arg, arg) | arg <- rargs]

    emit (CApp (CVar t fn) args) = do
        -- Is the function even defined? fn' will be the expected type
        Var fn' <- emit $ CVar t fn
        args' <- mapM emit args
        let f = App fn' args'
        void $ ty f
        return f

    emit (CApp f _) = serr $ "Expected function; got `" <> render f <> "`"

    emit (CLet _t var val) =
      emit val >>= \case
          -- `a = 1`
          Lit lit -> do
              ref <- ty (Lit lit) >>= \t' -> return $ Ref var 0 t' Global
              extend var ref
              return $ Let ref $ Lit lit

          -- `a = b`
          Var ref -> do
              extend var ref
              return $ Var ref

          -- `f x = x`
          Lam ref' args body -> do
              let ref = ref' {rname = var}
              extend var ref
              return $ Lam ref args body

          -- `a = sum 1 2`
          f@(App fn args) -> do
              ref <- ty f >>= \t -> return $ fn {rname = var, rty = t}
              extend var ref
              return $ Let ref $ App fn args

          err -> serr $ "Malformed local variable " <> (toS . render $ err)

-- | Rename core to avoid shadowing
rename :: [Core] -> Compiler [Core]
rename = return

-- | Verify the core
--
-- 1. Ensure redundant types match
verify :: [Core] -> Compiler [Core]
verify = return

-- | Find free variables in Core
free :: [Core] -> Compiler [Ref]
free cs = concat <$> mapM emit cs
  where
    emit :: Core -> Compiler [Ref]
    emit Lit {}              = return []
    emit (Var ref)           = maybeToList <$> fetch (rname ref)
    emit (Lam _fn args body) = do
        mapM_ (\arg -> modify (Map.insert (rname arg) arg)) args
        concatMapM emit body
    emit (App ref args)      = do
      a <- maybeToList <$> fetch (rname ref)
      b <- concatMapM emit args
      return $ a ++ b
    emit (Let _ _)           = return []

-- * Aliases to errors raised by the compiler
--
-- | Throw a SyntaxError
serr :: Text -> Compiler a
serr = throwError . SyntaxError

-- | Halt and catch fire
panic :: Text -> Compiler a
panic = throwError . Panic
