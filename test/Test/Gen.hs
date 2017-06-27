{-# LANGUAGE OverloadedStrings #-}
module Test.Gen (tests) where

import Protolude

import Olifant
import Olifant.Core

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LLVM Code generator" [vars, fn]

n :: Int -> Core
n n' = Lit TInt (LNumber n')

t :: Core
t = Lit TBool (LBool True)

vars :: TestTree
vars = testCase "Global variables" $ pretty progn @?= Right gen
  where
    progn = Progn [Bind "x" (n 42) , Bind "y" t] (Var TInt "x")

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \@x = global i64 42\n\n\
          \@y = global i1 1"

fn :: TestTree
fn = testCase "Simple identity function" $
     pretty progn @?= Right gen

  where
    -- | Sample program `x -> x`
    progn :: Progn Tipe
    progn = Progn [Bind "id" (Lam (TArrow [TInt, TInt]) "x" $ Var TInt "x")]
        (App TInt (Var TInt "id") (n 1))

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i64 @id(i64 %x){\n\
          \entry:\n\
          \  ret i64 %x\n}"
