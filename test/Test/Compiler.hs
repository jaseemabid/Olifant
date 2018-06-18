{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler (tests) where

import Protolude hiding (intercalate, local)

import Olifant.Compiler (compile)
import Olifant.Core
import Olifant.Parser

import Data.Text        (intercalate, lines)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler" [t1, t2, t3, zombie, global, local]

t1 :: TestTree
t1 = testCase "Identity function" $ t source @?= Right core
  where
    source :: [Text]
    source = ["id x:i = x", "id 42"]

    core :: [Core]
    core = [ Lam
             Ref {rname = "id", ri = 0, rty = TInt :> TInt, rscope = Global}
             [Ref {rname = "x", ri = 0, rty = TInt, rscope = Local}]
             [Var Ref {rname = "x", ri = 0, rty = TInt, rscope = Local}]
           , App
             Ref {rname = "id", ri = 0, rty = TInt :> TInt, rscope = Global}
             [Lit (Number 42)]
           ]

t2 :: TestTree
t2 = testCase "Const function" $ t source @?= Right core
  where
    source :: [Text]
    source = ["c x:i = 1", "c 42"]

    core :: [Core]
    core = [ Lam
             Ref {rname = "c", ri = 0, rty = TInt :> TInt, rscope = Global}
             [Ref {rname = "x", ri = 0, rty = TInt, rscope = Local}]
             [Lit (Number 1)]
           , App
             Ref {rname = "c", ri = 0, rty = TInt :> TInt, rscope = Global}
             [Lit (Number 42)]
           ]

t3 :: TestTree
t3 = testCaseSteps "Arity checks" $ \step -> do
    step "Fewer arguments"
    t ["f a:i b:i = 0", "f 1"] @?= Left TyError {expr = fewer}

    step "Surplus arguments"
    t ["f a:i b:i = 0", "f 1 2 3"] @?= Left TyError {expr = surplus}
  where
    f :: Ref
    f = Ref { rname = "f", ri = 0, rty = TInt :> TInt :> TInt
            , rscope = Global}

    fewer :: Core
    fewer = App f [Lit $ Number 1]

    surplus :: Core
    surplus = App f [Lit $ Number 1, Lit $ Number 2, Lit $ Number 3]

zombie :: TestTree
zombie = testCase "Find undefined variables" $ do
    t ["f x = p", "#t"] @?= Left (UndefinedError "p")
    t ["f x = g 42", "#t"] @?= Left (UndefinedError "g")

global :: TestTree
global = testCase "Global Variables" $
    t ["i = 1", "j = #t", "f a:i b:b = 42", "f i j"] @?= Right core
  where
    core = [ Let Ref {rname = "i", ri = 0, rty = TInt, rscope = Global} $ Lit $ Number 1
           , Let Ref {rname = "j", ri = 0, rty = TBool, rscope = Global} $ Lit $ Bool True
           , Lam Ref { rname = "f"
                     , ri = 0
                     , rty = TInt :> TBool :> TInt
                     , rscope = Global}
             [ Ref {rname = "a", ri = 0, rty = TInt, rscope = Local}
             , Ref {rname = "b", ri = 0, rty = TBool, rscope = Local}]
             [Lit $ Number 42]
           , App Ref {rname = "f"
                     , ri = 0
                     , rty = TInt :> TBool :> TInt
                     , rscope = Global}
             [ Var Ref {rname = "i", ri = 0, rty = TInt, rscope = Global}
             , Var Ref {rname = "j", ri = 0, rty = TBool, rscope = Global}]]

local :: TestTree
local = testCase "Support local variables" $ do
    src <- lines <$> readFile "examples/sum.ol"
    t src @?= Right core
  where
    core = [ Lam add
             [ Ref {rname = "a", ri = 0, rty = TInt, rscope = Local}
             , Ref {rname = "b", ri = 0, rty = TInt, rscope = Local}
             , Ref {rname = "c", ri = 0, rty = TInt, rscope = Local}
             ]
             [ Let
               Ref {rname = "t", ri = 0, rty = TInt, rscope = Local}
               (App
                 Ref
                 { rname = "sum"
                 , ri = 0
                 , rty = TInt :> (TInt :> TInt)
                 , rscope = Extern
                 }
                 [ Var Ref {rname = "a", ri = 0, rty = TInt, rscope = Local}
                 , Var Ref {rname = "b", ri = 0, rty = TInt, rscope = Local}
                 ])
             , App
               Ref
               { rname = "sum"
               , ri = 0
               , rty = TInt :> (TInt :> TInt)
               , rscope = Extern
               }
               [ Var Ref {rname = "t", ri = 0, rty = TInt, rscope = Local}
               , Var Ref {rname = "c", ri = 0, rty = TInt, rscope = Local}
               ]
             ]
           , App add [Lit (Number 1), Lit (Number 2), Lit (Number 3)]]

    add :: Ref
    add = Ref { rname = "add"
              , ri = 0
              , rty = TInt :> (TInt :> (TInt :> TInt))
              , rscope = Global
              }

-- Helpers
t :: [Text] -> Either Error [Core]
t code = parse (s code) >>= compile

s :: [Text] -> Text
s = intercalate "\n"
