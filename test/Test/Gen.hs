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
      Let (Ref "x" TInt) (Lit (LNumber 42))
    , Let (Ref "y" TBool) (Lit (LBool True))
    ] @?= toS gen

  where
    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \@x = global i64 42\n\n\
          \@y = global i1 1"

use :: TestTree
use = testCase "Use global variable" $
  pretty progn @?= toS gen

  where
    -- | Sample program `x -> x`
    progn :: [Core]
    progn = [Lam f (Ref "a" TBool) $ Var (Ref "b" TBool)
           , Lam g (Ref "a" TInt) $ Var (Ref "i" TInt)]

    f :: Ref Tipe
    f = Ref "f" (TArrow [TBool, TBool])

    g :: Ref Tipe
    g = Ref "g" (TArrow [TInt, TInt])

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i1 @f(ii %a){\n\
          \entry:\n\
          \  ret i1 %b\n}\
          \define external ccc i64 @f(i64 %a){\n\
          \entry:\n\
          \  ret i64 %i\n}"

fn :: TestTree
fn = testCase "Simple identity function" $ pretty [progn] @?= toS gen

  where
    -- | Sample program `x -> x`
    progn :: Core
    progn = Lam id' arg' body'
      where
        id' :: Ref Tipe
        id' = Ref "id" (TArrow [TInt, TInt])

        arg' :: Ref Tipe
        arg' = Ref "x" TInt

        body' :: Core
        body' = Var $ Ref "x" TInt

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i64 @id(i64 %x){\n\
          \entry:\n\
          \  ret i64 %x\n}"

constfn :: TestTree
constfn = testCase "Const function returning false" $
  pretty [progn] @?= toS gen

  where
    -- | Sample program `x -> x`
    progn :: Core
    progn = Lam sad arg' body'

    sad :: Ref Tipe
    sad = Ref "sad" (TArrow [TInt, TBool])

    arg' :: Ref Tipe
    arg' = Ref "y" TInt

    body' :: Core
    body' = Lit (LBool False)

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n\
          \define external ccc i1 @sad(i64 %y){\n\
          \entry:\n\
          \  ret i1 0\n}"
