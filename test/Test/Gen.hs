{-# LANGUAGE OverloadedStrings #-}

module Test.Gen where

import Protolude

import Olifant
import Olifant.Core

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "LLVM Code generator" [assign, use, fn, constfn]

assign :: TestTree
assign = testCase "Global variables" $
    pretty [
        Bind "x" (Lit TInt (LNumber 42))
      , Bind "y" (Lit TBool (LBool True))
      ] @?= Right gen

  where
    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \@x = global i64 42\n\n\
          \@y = global i1 1"

use :: TestTree
use = testCase "Use global variable" $
    pretty progn @?= Right gen

  where
    -- | Sample program `x -> x`
    progn :: Progn
    progn = [Bind "f" f, Bind "g" g]

    f :: Core
    f = Lam (TArrow [TBool, TBool]) "a" $ Var TBool "b"

    g :: Core
    g = Lam (TArrow [TInt, TInt]) "a" $ Var TInt "i"

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i1 @f(i1 %a){\n\
          \entry:\n\
          \  ret i1 %b\n}\n\n\
          \define external ccc i64 @g(i64 %a){\n\
          \entry:\n\
          \  ret i64 %i\n}"

fn :: TestTree
fn = testCase "Simple identity function" $
     pretty progn @?= Right gen

  where
    -- | Sample program `x -> x`
    progn :: Progn
    progn = [Bind "id" (Lam (TArrow [TInt, TInt]) "x" $ Var TInt "x")]

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i64 @id(i64 %x){\n\
          \entry:\n\
          \  ret i64 %x\n}"

constfn :: TestTree
constfn = testCase "Const function returning false" $
  pretty progn @?= Right gen

  where
    -- | Sample program `x -> x`
    progn :: Progn
    progn = [Bind "sad" sad]

    sad :: Core
    sad = Lam (TArrow [TInt, TBool]) "y" (Lit TBool (LBool False))

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i1 @sad(i64 %y){\n\
          \entry:\n\
          \  ret i1 0\n}"
