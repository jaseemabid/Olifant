{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import           Olifant.Core

import qualified Data.Map.Strict as Map
import           Protolude       hiding (Const, panic)

-- | Env maps textual representation to a refined reference
type Env = Map Text Ref

-- | Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- * Exposed API

-- | Top level API of the module

compile :: [Calculus] -> Either Error [Core]
compile ls = evalM (structure ls >>= infer >>= rename >>= verify) mempty

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

-- | Get the type of a *well typed* core expression
--
-- This function is reliable only after the infer pass; encode this with types.
ty :: Core -> Ty
ty (Lit (Number _)) = TInt
ty (Lit (Bool _))   = TBool
ty (Var ref)        = rty ref
ty (Lam ref _ _)    = rty ref
ty (App ref _)      = rty ref
ty (Let ref _)      = rty ref

-- | Return type of a type
retT :: Ty -> Ty
retT (TArrow _ tb) = retT tb
retT t             = t

-- | Arguments of a type
argT :: Ty -> Ty
argT (TArrow ta _) = ta
argT t             = t

-- | Arguments of a type
arity :: Ty -> Int
arity (TArrow _ t) = 1 + arity t
arity _            = 0

-- | Make function type out of the argument types & body type
unapply :: Ty -> [Ty] -> Ty
unapply = foldr TArrow

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
    decl c = serr $ "Expected let expression, got " <> show c <> " instead"

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
            emit body

        -- Compute the type of the function from args and body
        -- [TODO] - Inline `unapply` function
        let t = unapply (ty body') (map rty rargs)
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

        --  Arity check
        when (arity (rty fn') /= length args') $
          throwError TyError {expr = App fn' args'}

        -- Type matching
        case apply (rty fn') (map ty args') of
            Just _  -> return $ App fn' args'
            Nothing -> throwError TyError {expr = App fn' args'}
        where
          -- [TODO] - Change type to Either Ty TyError
          apply :: Ty -> [Ty] -> Maybe Ty
          apply ta [] = return ta
          apply (TArrow ta tb) (tz:ts)
            | tz == ta = apply tb ts
            | otherwise = Nothing
          apply _ _ = Nothing

    emit (CApp _ _ ) =
      throwError $ SyntaxError "Higher order functions not supported _yet_"

    emit (CLet _t var val) =
      emit val >>= \case
        Lam ref' args body -> do
          let r = ref' {rname = var}
          extend var r
          return $ Lam r args body
        Lit lit -> do
          let r = Ref var 0 (ty $ Lit lit) Global
          extend var r
          return $ Let r lit
        err -> throwError $ SyntaxError $ show err

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
        emit body
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
