{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler where

import           Protolude hiding (cast)

import qualified Olifant.Calculus as CL
import           Olifant.Compiler (cast, free)
import           Olifant.Core
import           Olifant.Parser (read)

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [translate, zombie]

-- |  Compile calculus to core
translate :: TestTree
translate = testCase "Trivial function translation" $ do
    read source @?= Right [calc]
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
    (free . cast) <$$> read "/x.p" @?= Right [[Ref "p"]]
    (free . cast) <$$> read "/x.x" @?= Right [[]]

    (free . cast) <$$> read "/x.42" @?= Right [[]]

    (free . cast) <$$> read "/x.f 42" @?= Right [[Ref "f"]]
    (free . cast) <$$> read "/x.x 42" @?= Right [[]]

    (free . cast) <$$> read "/x.42" @?= Right [[]]

-- * Helpers

-- | Nested functors

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$>
