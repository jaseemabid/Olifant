{-|
Module      : Olifant.Core
Description : Core data structures of the compiler
-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Olifant.Core where

import Protolude
import Prelude (last, init)

--  - https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
--  - http://blog.ezyang.com/2013/05/the-ast-typing-problem/

-- Known issues
-- [TODO] - Ensure non empty [] for TArrow

newtype Ref = Ref {rname :: Text}
    deriving (Eq, Ord, Show)

data Literal = LNumber Int | LBool Bool
    deriving (Eq, Show)

data Tipe = TInt | TBool | TArrow [Tipe]
    deriving (Eq, Show)

-- | An annotated lambda calculus expression
data Expr a
    = Var a Ref
    | Lit a Literal
    | App a (Expr a) (Expr a)
    | Lam a Ref (Expr a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Top level binding of a lambda calc expression to a name
data Bind a = Bind Ref (Expr a)

-- | A program is a list of typed bindings
type Progn = [Bind Tipe]

-- | Untyped calculus
type CoreUT = Expr ()

-- | Typed calculus
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
