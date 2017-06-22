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

import Protolude hiding ((<>), show)
import Prelude (show, last, init)
import Data.String
import Data.Foldable (foldr1)
import Text.PrettyPrint

--  - https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
--  - http://blog.ezyang.com/2013/05/the-ast-typing-problem/

-- Known issues
-- [TODO] - Ensure non empty [] for TArrow

newtype Ref = Ref {rname :: Text}
    deriving (Eq, Ord)

data Literal = LNumber Int | LBool Bool
    deriving (Eq, Show)

data Tipe = TInt | TBool | TArrow [Tipe]
    deriving (Eq)

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
newtype Progn = Progn [Bind Tipe]

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

-- Ed Kmett says I should not use UndecidableInstances to avoid this
-- boilerplate, so I'm gonna do that. `D a => Show a` looked so promising :/
--

instance Show Ref where
    show (Ref a) = show a

instance Show Tipe where
    show = render . p

instance Show Core where
    show = render . p

instance Show CoreUT where
    show = render . p

-- instance Show Progn where
--    show = render . p

-- * Pretty printer
--
-- These functions are in core to avoid circular dependency between core and
-- pretty printer module.

arrow :: Doc
arrow = text "->"

class D a where
    p :: a -> Doc

instance D Ref where
    p (Ref r) = text $ toS r

instance D Tipe where
    p TInt = "i"
    p TBool = "b"
    p (TArrow ts) = foldr1 (\a b -> a <+> arrow <+> b) $ map p ts

instance D (Expr ()) where
    p (Var _ (Ref n)) = text $ toS n
    p (Lit _ (LNumber n)) = int n
    p (Lit _ (LBool True)) = "#t"
    p (Lit _ (LBool False)) = "#t"
    p (App _ a b) = p a <+> p b
    p (Lam _ (Ref a) b) = char 'λ' <> text (toS a) <> char '.' <> p b

instance D (Expr Tipe) where
    p (Var t (Ref n)) = text (toS n) <+> colon <+> p t
    p (Lit _ (LNumber n)) = int n
    p (Lit _ (LBool True)) = "#t"
    p (Lit _ (LBool False)) = "#t"
    p (App t a b) = p a <+> p b <+> colon <+> p t
    p (Lam t (Ref a) b) = char 'λ' <> text (toS a) <> p t <> char '.' <> p b

instance D (Bind ()) where
    p (Bind r val) = p r <> p val

instance D (Bind Tipe) where
    p (Bind r val) = p r <> p val

instance D a => D [a] where
    p xs = vcat $ map p xs
