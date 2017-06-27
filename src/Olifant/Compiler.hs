{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}

{-# LANGUAGE OverloadedStrings #-}

module Olifant.Compiler where

import qualified Olifant.Calculus as C
import           Olifant.Core
import           Prelude          (String)
import           Protolude        hiding (uncurry)

-- Compiler is an Olifant monad with state set to `Env`
type Compiler a = Olifant Env a

-- Env is a simple list of references
type Env = [Ref]

-- | Top level API of the module
compile :: [C.Calculus] -> Either Error (Progn Tipe)
compile ls = evalM (translate ls >>= typecheck) []

-- | Compile a series of Calculus expressions into untyped core bindings
--
-- Input program should be a series of let bindings and one expression in the
-- end. This is the first transformation by the compiler.
translate :: [C.Calculus] -> Compiler (Progn ())
translate cs = case unsnoc cs of
    Just (decls, main) -> Progn <$> mapM top decls <*> t1 main
    Nothing            -> serr $ toS (show cs :: String)

  where
    -- | Translate a top level expression into a declaration
    top :: C.Calculus -> Compiler (Bind ())
    top (C.Let var val) = t1 val >>= return . Bind (Ref var)
    top l = serr $ "Expected let expression, got " <> show l <> " instead"

    -- | Translate a nested expression into untyped core
    t1 :: C.Calculus -> Compiler CoreUT
    t1 (C.Var a)      = return $ Var unit $ Ref a
    t1 (C.Number n)   = return $ Lit unit (LNumber n)
    t1 (C.Bool b)     = return $ Lit unit (LBool b)
    t1 (C.App fn arg) = App unit <$> t1 fn <*> t1 arg
    t1 (C.Lam n b)    = Lam unit (Ref n) <$> t1 b
    t1 (C.Let _ _)    = throwError $ SyntaxError "Nested let expression"

-- | Type check!
--
-- This is pretty stupid,naive and wrong. Implement Hindley-Milner soon
typecheck :: Progn () -> Compiler (Progn Tipe)
typecheck (Progn decls main) =
    Progn <$> mapM top decls <*> t1 main
  where
    -- Type check a top level expression
    top  :: Bind () -> Compiler (Bind Tipe)
    top b@(Bind _ expr) = hm expr >>= \tipe -> return $ fmap (const tipe) b

    t1 :: CoreUT -> Compiler Core
    t1 e = hm e >>= \tipe -> return $ fmap (const tipe) e

    hm :: CoreUT -> Compiler Tipe
    hm (Var () _ref)        = return (TArrow [TInt, TInt])
    hm (Lit () (LNumber _)) = return TInt
    hm (Lit () (LBool _))   = return TBool
    hm (App () _e1 _e2)     = return TInt
    hm (Lam () _ref body)   = hm body >>= return . uncurry TInt

-- | Find free variables; typed or untyped program
free :: Progn a -> Compiler [Ref]
free (Progn decls main) = concat <$> liftA2 (:) (f main) (mapM g decls)
  where

    g :: Bind a -> Compiler [Ref]
    g (Bind ref val) = put [ref] >> f val

    f :: Expr a -> Compiler [Ref]
    f (Var _ x) = get >>= \acc -> return $ if x `elem` acc then [] else [x]
    f Lit{} = return []
    f (App _t fn exp') = return (++) <*> f fn <*> f exp'
    f (Lam _fn arg body) = modify (\s -> arg: s) >> f body


-- | Errors raised by the compiler
terr :: Text -> Compiler a
terr = throwError . TipeError

serr :: Text -> Compiler a
serr = throwError . SyntaxError
