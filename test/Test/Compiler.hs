{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler where

import           Protolude hiding (cast)

import qualified Olifant.Calculus as CL
import           Olifant.Compiler (cast, undef)
import qualified Olifant.Core as CO
import           Olifant.Parser (read)

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [translate, zombie]

-- * Compile calculus to core

translate :: TestTree
translate = testCase "Trivial function translation" $ do
    read source @?= Right [calc]
    cast calc @?= core

  where
    source :: Text
    source = "/x./y.42"

    calc :: CL.Calculus
    calc = CL.Lam "x" (CL.Lam "y" (CL.Number 42))

    core :: CO.CoreUT
    core = CO.Lam t x (CO.Lam t y (CO.Lit (CO.LNumber 42)))
      where
        t = CO.Ref "~" CO.unit
        x = CO.Ref "x" CO.unit
        y = CO.Ref "y" CO.unit

zombie :: TestTree
zombie = testCase "Find undefined variables" $ do
    (undef . cast) <$$> read s1 @?= Right [Left "p"]
    (undef . cast) <$$> read s1 @?= Right [CL.Lam (CO.Ref "x" CO.unit)]
  where
    s1 :: Text
    s1 = "/x.p"

    s2 :: Text
    s2 = "/x.42"

-- * Helpers

-- | Nested functors

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$>
