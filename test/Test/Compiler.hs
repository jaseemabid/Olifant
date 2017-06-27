{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler (tests) where

import Protolude

import qualified Olifant.Calculus as CL
import           Olifant.Compiler
import           Olifant.Core
import           Olifant.Parser   (parse)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [t1, zombie]

-- |  Compile calculus to core
t1 :: TestTree
t1 = testCase "Trivial function translation" $ do
    parse source @?= Right calc
    compile calc @?= Right progn

  where
    source :: Text
    source = "let c = /x.1; c 42"

    calc :: [CL.Calculus]
    calc = [CL.Let "c" (CL.Lam "x" (CL.Number 1))
           , CL.App (CL.Var "c") (CL.Number 42)]

    progn :: Progn Tipe
    progn = Progn [Bind "c" $ Lam TInt "x" (n 1)] (App TInt (Var TInt "c") (n 42))

zombie :: TestTree
zombie = testCase "Find undefined variables" $ do
    t "/x.p"    @?= Right ["p"]
    t "let p = 1; /x.p"    @?= Right []
    t "/x.x"    @?= Right []
    t "/x.42"   @?= Right []
    t "/x.f 42" @?= Right ["f"]
    t "let f = id; /x.f 42" @?= Right ["id"]
    t "/x.x 42" @?= Right []
    t "/x.42"   @?= Right []
  where
    t :: Text -> Either Error [Ref]
    t e = evalM (translate (fromRight $ parse e) >>= free) []

fromRight :: Either a b -> b
fromRight (Right a) = a

n :: Int -> Core
n n' = Lit TInt (LNumber n')
