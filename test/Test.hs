module Main where

import LLVM.Pretty (ppllvm)
import Olifant
import Olifant.Core
import Protolude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [simple]

simple :: TestTree
simple = testCase "Simple assignment" $ (ppllvm $ compile [progn]) @?= toS gen
  where

    -- | Sample program `x = 1`
    progn :: Core
    progn = Let x one'
      where
        x :: Ref Tipe
        x = Ref "x" TInt

        one' :: Core
        one' = Lit (LNumber 1 TInt)

    gen :: Text
    gen = "; ModuleID = 'calc'\n\n@x = global i64 1"
