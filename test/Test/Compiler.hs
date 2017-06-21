{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler where

import           Protolude hiding (cast, head)
import           Prelude (head)

import qualified Olifant.Calculus as CL
import           Olifant.Compiler (cast, free)
import           Olifant.Core
import           Olifant.Parser (parse)

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [translate, zombie]

-- |  Compile calculus to core
translate :: TestTree
translate = testCase "Trivial function translation" $ do
    parse source @?= Right [calc]
    cast calc @?= core

  where
    source :: Text
    source = "/x./y.42"

    calc :: CL.Calculus
    calc = CL.Lam "x" (CL.Lam "y" (CL.Number 42))

    core :: CoreUT
    core = Lam unit x (Lam unit y (Lit unit (LNumber 42)))
      where
        x = Ref "x"
        y = Ref "y"

zombie :: TestTree
zombie = testCase "Find undefined variables" $ do
    free (cast $ parse1 "/x.p")   @?= [Ref "p"]
    free (cast $ parse1 "/x.x")    @?= []
    free (cast $ parse1 "/x.42")   @?= []
    free (cast $ parse1 "/x.f 42") @?= [Ref "f"]
    free (cast $ parse1 "/x.x 42") @?= []
    free (cast $ parse1 "/x.42")   @?= []

-- * Helpers

parse1 :: Text -> CL.Calculus
parse1 t = head $ fromRight $ parse t

fromRight :: Either a b -> b
fromRight (Right a) = a
