{-# LANGUAGE OverloadedStrings #-}

module Test.Gen (tests) where

import Protolude hiding (intercalate)

import Data.Text        (intercalate)
import Olifant.Compiler (compile)
import Olifant.Core     (Ty (..))
import Olifant.Gen      (gen, native)
import Olifant.Parser   (parse)

import LLVM.AST.Type

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "LLVM Code generator" [nt, t1, t2, t3, t4, t5]

-- | Validate the mapping from lang types to llvm types.
nt :: TestTree
nt = testCaseSteps "Native types" $ \step -> do

    step "Simple primitive types"

    native TInt @?= i64
    native TBool @?= i1

    -- Damn! I don't have a constant functions!

    step "Unary identity Function"

    native (TInt :> TInt) @?= FunctionType { argumentTypes = [i64]
                                           , resultType = i64
                                           , isVarArg = False
                                           }

    step "Binary Function"

    native (TInt :> TInt :> TInt) @?= FunctionType { argumentTypes = [i64, i64]
                                                   , resultType = i64
                                                   , isVarArg = False
                                                   }

t1 :: TestTree
t1 = testCase "Identity function" $ do
    ir <- readFile "test/ll/id.ll"
    t ["id x:i = x", "id 42"] ir

t2 :: TestTree
t2 = testCase "Const function" $ do
    ir <- readFile "test/ll/const.ll"
    t ["c x:i = 1", "c 42"] ir

t3 :: TestTree
t3 = testCase "Global variables" $ do
    ir <- readFile "test/ll/global.ll"
    t ["i = 1", "j = #t", "f a:i b:b = 42", "f i j"] ir

t4 :: TestTree
t4 = testCase "Shadow variables" $ do
    ir <- readFile "test/ll/shadow.ll"
    t ["a = 1", "f a:i = a", "f a"] ir

t5 :: TestTree
t5 = testCase "Native function" $ do
    ir <- readFile "test/ll/sum.ll"
    t ["sum 4 5"] ir

t :: [Text] -> Text -> IO ()
t code ir = case compile <$> parse (intercalate "\n" code) of
    Right (Right x) -> gen x >>= \l -> l @?= Right ir
    err             -> assertFailure $ show err
