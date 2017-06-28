{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler (tests) where

import Protolude hiding (cast)

import Olifant.Compiler (compile)
import Olifant.Core
import Olifant.Parser

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [t1, t2, zombie]

t1 :: TestTree
t1 = testCase "Identity function" $
    c source @?= Right progn
  where
    source :: Text
    source = "let id = /k:i.k; id 42"

    progn :: Progn
    progn = Progn [Bind "id" $ Lam id "k" (Var TInt "k")] (App TInt (Var id "id") (n 42))

    id :: Tipe
    id = TArrow TInt TInt

t2 :: TestTree
t2 = testCase "Const function" $
    c source @?= Right progn
  where
    source :: Text
    source = "let c = /x:i.1; c 42"

    progn :: Progn
    progn = Progn [Bind "c" $ Lam id "x" (n 1)] (App TInt (Var id "c") (n 42))

    id :: Tipe
    id = TArrow TInt TInt

zombie :: TestTree
zombie = testCase "Find undefined variables" $ do
    c "/x.p"    @?= Left (UndefinedError "p")
    c "/x.f 42" @?= Left (UndefinedError "f")
    c "let f = id; /x.f 42" @?= Left (UndefinedError "id")

n :: Int -> Expr
n n' = Lit TInt (LNumber n')

c :: Text -> Either Error Progn
c t = parse t >>= compile
