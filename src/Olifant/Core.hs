{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Olifant.Core where

import Protolude

-- Core
--
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
-- http://blog.ezyang.com/2013/05/the-ast-typing-problem/
--
-- // Intentionally presented without comments //
--

-- * Known issues
--
-- 1. Ref a -> a
-- 2. Function types is redundant with arg types and ret type
-- 3. Non empty [] for TArrow

data Ref a = Ref {rname :: Text, tipe :: Tipe}
    deriving (Show, Functor, Foldable, Traversable)

data Literal a = LNumber Integer | LBool Bool
    deriving (Show, Functor, Foldable, Traversable)

data Tipe = TInt | TBool | TArrow [Tipe]
    deriving (Show)

data Expr a
    = Var (Ref a)
    | Lit (Literal a)
    | App (Expr a) (Expr a)
    | Lam
      { name :: Ref a
      , arg :: Ref a
      , body :: Expr a
      }
    -- Let binds a global variable for now; fix the semantics
    | Let (Ref a) (Expr a)
    deriving (Show, Functor, Foldable, Traversable)

type CoreUT = Expr ()

type Core = Expr Tipe

-- * Aliases

unit :: ()
unit = ()
