{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Olifant.Core where

import Protolude
import Prelude (last, init)

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

data Ref a = Ref {rname :: Text, tipe :: a}
    deriving (Show, Functor, Foldable, Traversable)

data Literal a = LNumber Int | LBool Bool
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

-- * Type helpers

-- | Return type of a type
ret :: Tipe -> Tipe
ret (TArrow ts) = last ts
ret t = t

-- | Arguments of a type
args :: Tipe -> Tipe
args (TArrow ts) = TArrow $ init ts
args t = t

-- * Aliases

unit :: ()
unit = ()
