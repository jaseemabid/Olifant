{-|
Module      : Olifant.Core
Description : Core data structures of the compiler
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Olifant.Core where

import Protolude hiding ((<>))
import Prelude (Show(..), last, init)
import Data.String
import Data.Foldable (foldr1)
import Text.PrettyPrint

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
    deriving (Eq, Functor, Foldable, Traversable)

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

-- * Instance declarations

instance IsString Ref where
    fromString x = Ref $ toS x

instance Show Core where
    show = render . p

instance Show  CoreUT where
    show = render . pu

-- * Pretty printer
--
-- These functions are in core to avoid circular dependency between core and
-- pretty printer module.

arrow :: Doc
arrow = text "->"

-- | Pretty print a type
pt :: Tipe -> Doc
pt TInt = "i"
pt TBool = "b"
pt (TArrow ts) = foldr1 (\a b -> a <+> arrow <+> b) $ map pt ts

-- | Pretty print the untyped core
pu :: CoreUT -> Doc
pu (Var _ (Ref n)) = text $ toS n
pu (Lit _ (LNumber n)) = int n
pu (Lit _ (LBool True)) = "#t"
pu (Lit _ (LBool False)) = "#t"
pu (App _ a b) = pu a <+> pu b
pu (Lam _ (Ref a) b) = char 'λ' <> text (toS a) <> char '.' <> pu b

-- | Pretty print typed core
p :: Core -> Doc
p (Var t (Ref n)) = text (toS n) <+> colon <+> pt t
p (Lit _ (LNumber n)) = int n
p (Lit _ (LBool True)) = "#t"
p (Lit _ (LBool False)) = "#t"
p (App t a b) = p a <+> p b <+> colon <+> pt t
p (Lam t (Ref a) b) = char 'λ' <> text (toS a) <> pt t <> char '.' <> p b
