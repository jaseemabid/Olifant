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
tests = testGroup "LLVM Code generator" [assign, fn, constfn]

assign :: TestTree
assign = testCase "Global variable assignment" $ do
  pretty [Let x num] @?= toS genInt
  pretty [Let x yes] @?= toS genBool

  where
    x :: Ref Tipe
    x = Ref "x" TInt

    num :: Core
    num = Lit (LNumber 42)

    yes :: Core
    yes = Lit (LBool True)

    genInt :: Text
    genInt = "; ModuleID = 'calc'\n\n@x = global i64 42"

    genBool :: Text
    genBool = "; ModuleID = 'calc'\n\n@x = global i1 1"

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
