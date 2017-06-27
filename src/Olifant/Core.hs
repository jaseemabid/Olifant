{-|
Module      : Olifant.Core
Description : Core data structures of the compiler
-}

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Olifant.Core where

import Data.Foldable    (foldr1)
import Data.String
import Prelude          (init, last, show)
import Protolude        hiding (show, uncurry, (<>))
import Text.Parsec      (ParseError)
import Text.PrettyPrint

--  - https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
--  - http://blog.ezyang.com/2013/05/the-ast-typing-problem/

-- Known issues
-- [TODO] - Ensure non empty [] for TArrow

newtype Ref = Ref {rname :: Text}
    deriving (Eq, Ord)

data Literal = LNumber Int | LBool Bool
    deriving (Eq, Show)

-- [TODO] - Make Tipe a `Traversable` and `IsList`
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
  deriving (Eq, Functor)

-- | A program is a list of bindings and an expression
data Progn a = Progn [Bind a] (Expr a)
  deriving (Eq, Functor)

-- | Untyped calculus
type CoreUT = Expr ()

-- | Typed calculus
type Core = Expr Tipe

-- * Type helpers

-- | Return type of a type
ret :: Tipe -> Tipe
ret (TArrow ts) = last ts
ret t           = t

-- | Arguments of a type
args :: Tipe -> Tipe
args (TArrow ts) = TArrow $ init ts
args t           = t

-- | Uncurry; convert a type to a function that returns that type
uncurry :: Tipe -> Tipe -> Tipe
uncurry t (TArrow ts) = TArrow $ t:ts
uncurry t1 t2         = TArrow [t1, t2]

-- * Aliases

unit :: ()
unit = ()

-- * Error handling and state monad

-- | Errors raised by the compiler
--
data Error =
    GenError Text
  | Panic Text
  | ParseError ParseError
  | SyntaxError Text
  | TipeError Text
    deriving (Eq, Show)

-- Olifant Monad
--
-- A `State + Error + IO` transformer with Error type fixed to `Error`
newtype Olifant s a = Olifant
    { runOlifant :: StateT s (Except Error) a
    } deriving (Functor, Applicative, Monad, MonadError Error, MonadState s)

-- | Run a computation in olifant monad with some state and return the result
evalM :: Olifant s a -> s -> Either Error a
evalM c s = runIdentity $ runExceptT $ evalStateT (runOlifant c) s

-- | Run a computation in olifant monad with some state and return new state
execM :: Olifant s a -> s -> Either Error s
execM c s = runIdentity $ runExceptT $ execStateT (runOlifant c) s

-- * Instance declarations

instance IsString Ref where
    fromString x = Ref $ toS x

-- Ed Kmett says I should not use UndecidableInstances to avoid this
-- boilerplate, so I'm gonna do that. `D a => Show a` looked so promising :/

instance Show Ref where
    show (Ref a) = show a

instance Show Tipe where
    show = render . p

instance Show Core where
    show = render . p

instance Show CoreUT where
    show = render . p

instance Show (Bind ()) where
    show = render . p

instance Show (Bind Tipe) where
    show = render . p

instance Show (Progn ()) where
  show (Progn ps e) = unlines $ map show ps ++ [show e]

instance Show (Progn Tipe) where
  show (Progn ps e) = unlines $ map show ps ++ [show e]

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

-- [TODO] - Fix type pretty printer for higher order functions
instance D Tipe where
    p TInt        = "i"
    p TBool       = "b"
    p (TArrow ts) = foldr1 (\a b -> a <+> arrow <+> b) $ map p ts

instance D (Expr ()) where
    p (Var _ (Ref n))       = text $ toS n
    p (Lit _ (LNumber n))   = int n
    p (Lit _ (LBool True))  = "#t"
    p (Lit _ (LBool False)) = "#t"
    p (App _ a b)           = p a <+> p b
    p (Lam _ (Ref a) b)     = char 'λ' <> text (toS a) <> char '.' <> p b

instance D (Expr Tipe) where
    p (Var t (Ref n)) = text (toS n) <> colon <> p t
    p (Lit _ (LNumber n)) = int n
    p (Lit _ (LBool True)) = "#t"
    p (Lit _ (LBool False)) = "#t"
    p (App t a b) = p a <+> p b <> colon <> p t
    p (Lam t (Ref a) b) = char 'λ' <> colon <> p t <+> text (toS a) <> char '.' <> p b

instance D (Bind ()) where
    p (Bind r val) = text "let" <+> p r <+> char '=' <+> p val

-- [TODO] - Add type to pretty printed version of let binding
instance D (Bind Tipe) where
    p (Bind r val) = text "let" <+> p r <+> char '=' <+> p val

instance D a => D [a] where
    p xs = vcat $ map p xs
