{-|
Module      : Olifant.Core
Description : Core data structures of the compiler
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Olifant.Core where

import Data.String
import Prelude          (Show (..))
import Protolude        hiding (show, uncurry, (<>))
import Text.Parsec      (ParseError)
import Text.PrettyPrint

--  - https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
--  - http://blog.ezyang.com/2013/05/the-ast-typing-problem/

-- | Code generator treats local and global variables differently
data Scope = Local | Global
  deriving (Eq, Ord)

-- | A reference; an embedded data structure avoids the need for a symbol table
data Ref = Ref {rname :: Text, rtipe :: Tipe, scope :: Scope}
    deriving (Eq, Ord)

data Literal = LNumber Int | LBool Bool
    deriving Eq

-- [TODO] - Replace TArrow with ~>
data Tipe = TUnit | TInt | TBool | TArrow Tipe Tipe
    deriving (Eq, Ord)

-- | An annotated lambda calculus expression
data Expr
    = Var Ref
    | Lit Literal
    | App Tipe Expr Expr
    | Lam Tipe Ref Expr
    deriving Eq

-- | Top level binding of a lambda calc expression to a name
data Bind = Bind Ref Expr
  deriving (Eq)

-- | A program is a list of bindings and an expression
data Progn = Progn [Bind] Expr
  deriving (Eq)

-- * Type helpers
tipe :: Expr -> Tipe
tipe (Var (Ref _ t _)) = t
tipe (Lit (LNumber _)) = TInt
tipe (Lit (LBool _))   = TBool
tipe (App t _ _)       = t
tipe (Lam t _ _)       = t

-- | Return type of a type
retT :: Tipe -> Tipe
retT (TArrow _ tb) = retT tb
retT t             = t

-- | Arguments of a type
argT :: Tipe -> Tipe
argT (TArrow ta _) = ta
argT t             = t

-- | Convert a type to a function that returns that type
uncurry :: Tipe -> Tipe -> Tipe
uncurry = TArrow

-- | Apply a type to a function
--    > curry (TArrow [TInt, TBool]) TInt
--    TBool
curry :: Tipe -> Tipe -> Maybe Tipe
curry t (TArrow ta tb)
  | t == ta = Just tb
  | otherwise = Nothing
curry _ _ = Nothing

-- * Error handling and state monad

-- | Errors raised by the compiler
--
data Error =
    GenError Text
  | Panic Text
  | ParseError ParseError
  | SyntaxError Text
  | UndefinedError Ref
  | TipeError {expr :: Expr, expected :: Tipe, reality :: Tipe}
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
    fromString x = Ref (toS x) TUnit Local

-- Ed Kmett says I should not use UndecidableInstances to avoid this
-- boilerplate, so I'm gonna do that. `D a => Show a` looked so promising :/

instance Show Ref where
    show ref = show $ rname ref

instance Show Tipe where
    show = render . p

instance Show Expr where
    show = render . p

instance Show Bind where
    show = render . p

instance Show Progn where
    show (Progn ps e) = unlines $ map show ps ++ [show e]

-- * Pretty printer
--
-- These functions are in core to avoid circular dependency between core and
-- pretty printer module.

arrow, dot, lambda, lett :: Doc
arrow   = char '→'
lambda  = char 'λ'
dot     = char '.'
lett    = text "let"

class D a where
    p :: a -> Doc

instance D Ref where
    p (Ref n t Local)  = text (toS n) <> colon <> p t
    p (Ref n t Global) = char '@' <> text (toS n) <> colon <> p t

-- [TODO] - Fix type pretty printer for higher order functions
instance D Tipe where
    p TUnit          = "∅"
    p TInt           = "i"
    p TBool          = "b"
    p (TArrow ta tb) = p ta <> arrow <> p tb

instance D Expr where
    p (Var ref)           = p ref
    p (Lit (LNumber n))   = int n
    p (Lit (LBool True))  = "#t"
    p (Lit (LBool False)) = "#t"
    p (App _ f e)         = p f <+> p e
    p (Lam _ r e)         = lambda <> p r <> dot <> p e

-- [TODO] - Add type to pretty printed version of let binding
instance D Bind where
    p (Bind r val) = lett <+> p r <+> equals <+> p val

instance D a => D [a] where
    p xs = vcat $ map p xs
