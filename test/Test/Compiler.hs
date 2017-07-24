{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler
    ( tests
    ) where

import Protolude hiding (cast)

import Olifant.Compiler (compile)
import Olifant.Core
import Olifant.Parser

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [t1, t2, t3, zombie]

t1 :: TestTree
t1 = testCase "Identity function" $ c source @?= Right progn
  where
    source :: Text
    source = "let id = /k:i.k; id 42"
    progn :: Progn
    progn =
        Progn
            [Bind (g "id") $ Lam id [l "k"] (v "k")]
            (App TInt (Var $ tg "id" id) [Number 42])
    id :: Ty
    id = TArrow TInt TInt

t2 :: TestTree
t2 = testCase "Const function" $ c source @?= Right progn
  where
    source :: Text
    source = "let c = /x:i.1; c 42"
    progn :: Progn
    progn =
        Progn
            [Bind (tg "c" id) $ Lam id [l "x"] (Number 1)]
            (App TInt (Var $ tg "c" id) [Number 42])
    id :: Ty
    id = TArrow TInt TInt

t3 :: TestTree
t3 =
    testCaseSteps "Ensure arity" $ \step -> do
        step "Fewer arguments"
        c "let f = /a:i b:i.0; f 1" @?= Left TyError
        step "Surplus arguments"
        c "let f = /a:i b:i.0; f 1 2 3" @?= Left TyError

zombie :: TestTree
zombie =
    testCase "Find undefined variables" $ do
        c "/x.p" @?= Left (UndefinedError "p")
        c "/x.f 42" @?= Left (UndefinedError "f")
        c "let f = id; /x.f 42" @?= Left (UndefinedError "id")

-- Helpers
l :: Text -> Ref
l n = Ref n 0 TInt Local

g :: Text -> Ref
g n = Ref n 0 TInt Global

tg :: Text -> Ty -> Ref
tg n t = Ref n 0 t Global

v :: Text -> Expr
v = Var . l

c :: Text -> Either Error Progn
c t = parse t >>= compile
