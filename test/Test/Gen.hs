{-# LANGUAGE OverloadedStrings #-}
module Test.Gen (tests) where

import Protolude

import Olifant.Core
import Olifant.Gen  (llvm)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LLVM Code generator" [vars, fn]

n :: Int -> Expr
n n' = Lit TInt (LNumber n')

t :: Expr
t = Lit TBool (LBool True)

v :: Tipe -> Text -> Expr
v ta x = Var ta $ Ref x

vars :: TestTree
vars = testCase "Global variables" $
    llvm progn >>= \m -> m @?= Right gen
  where
    progn = Progn [Bind "x" (n 42) , Bind "y" t] (v TInt "x")

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \source_filename = \"<string>\"\n\
          \@x = global i64 42\n\n\
          \@y = global i1 1\n\n\
          \define i64 @main(){\n\
          \entry:\n\
          \  ret i64 %x\n}"

fn :: TestTree
fn = testCase "Simple identity function" $
     llvm progn >>= \m -> m @?= Right gen
  where
    -- | Sample program `x -> x`
    progn :: Progn
    progn = Progn [Bind "id" (Lam (TArrow TInt TInt) "x" $ v TInt "x")]
        (App TInt (v (TArrow TInt TInt) "id") (n 1))

    gen :: Text
    gen = "; ModuleID = 'calc'\n\
          \source_filename = \"<string>\"\n\n\
          \define i64 @id(i64 %x) {\n\
          \entry:\n\
          \  ret i64 %x\n\
          \}\n\n\
          \define i64 @main() {\n\
          \entry:\n\
          \  %0 = call i64 @id(i64 1)\n\
          \  ret i64 %0\n\
          \}\n"
