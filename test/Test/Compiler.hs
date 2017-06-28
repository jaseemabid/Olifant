{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler (tests) where

import Protolude hiding (cast)

import Olifant.Compiler (compile)
import Olifant.Core
import Olifant.Parser

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [t1, zombie]

-- |  Compile calculus to core
t1 :: TestTree
t1 = testCase "Trivial function translation" $
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
