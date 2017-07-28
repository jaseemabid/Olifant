{-|
Module      : Olifant.Core
Description : Core languages of the compiler
-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Olifant.Core where

import Protolude        hiding ((<>))
import Text.Parsec      (ParseError)
import Text.PrettyPrint

-- | All the known types
--
-- TUnit exists only as a placeholder for earlier partially typed languages. 2
-- kinds of types are ideal, but that would be so much confusion, name
-- collisions and boilerplate.
-- [TODO] - Replace TArrow with ~>
data Ty
    = TUnit
    | TInt
    | TBool
    | TArrow Ty Ty
    deriving (Eq, Ord, Show)

-- | literals, shared by all languages
data Literal = Bool Bool | Number Int
  deriving (Eq, Show)

-- | Calculus, the frontend language
--
-- 1. Extremely liberal, should be able to represent anything that is not a syntax error
-- 2. The language is not well typed
-- 3. Grammar is recursive
-- 4. Variables are not resolved, they are simple textual objects
-- 5. Let bindings can be unsafe; could be a redefinition, type error etc
-- 6. Higher order functions, functions with let bindings etc allowed
data Calculus
    = CLit Literal
    | CVar Ty Text
    | CLam Text [(Ty, Text)] Calculus
    | CApp Calculus [Calculus]
    | CLet Ty Text Calculus
    deriving (Eq, Show)

-- * Core

-- | Variable Scope
--
-- Code treats local and global variables differently. A scope type with and
-- without unit can be disambiguate at compile time, but that is for some other
-- day.
data Scope = Local | Global
    deriving (Eq, Ord, Show)

-- | A reference type
data Ref = Ref
    { rname :: Text   -- ^ User defined name of the variable
    , ri    :: Int    -- ^ Disambiguate the same name. Eg, a0, a1, a2
    , rty   :: Ty     -- ^ Type of the reference
    , rscope :: Scope -- ^ Is the variable local, global or unknown?
    } deriving (Eq, Ord, Show)

-- | The core language
--
-- Core is a reasonably verbose IR, suitable enough for most passes. It is
-- recursive, not perfectly type safe.
--
-- References:
--
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
-- http://blog.ezyang.com/2013/05/the-ast-typing-problem/
--
data Core
  = Lit Literal
  | Var Ref
  | Lam Ref [Ref] Core
  | App Ref [Core]
  | Let Ref Literal
  deriving (Eq, Show)

-- * The machine language
--
-- The obvious step before code generation.
-- 1. SSA, No compound expressions
-- 2. Not a recursive grammar
-- 3. Nothing that cant be trivially translated to LLVM
type Mach = Core

-- * Error handling and state monad
--
-- | Errors raised by the compiler
--
data Error
    = GenError Text
    | Panic Text
    | ParseError ParseError
    | SyntaxError Text
    | UndefinedError Text
    | TyError {expr :: Core}
    deriving (Eq, Show)

-- | Olifant Monad
--
-- A `State + Error` transformer with Error type fixed to `Error`
newtype Olifant s a = Olifant
    { runOlifant :: StateT s (Except Error) a
    } deriving (Applicative, Functor, Monad, MonadError Error, MonadState s)

-- | Run a computation in olifant monad with some state and return the result
evalM :: Olifant s a -> s -> Either Error a
evalM c s = runIdentity $ runExceptT $ evalStateT (runOlifant c) s

-- | Run a computation in olifant monad with some state and return new state
execM :: Olifant s a -> s -> Either Error s
execM c s = runIdentity $ runExceptT $ execStateT (runOlifant c) s

-- | Run a localized computation without spilling the state
localized :: MonadState s m => m b -> m b
localized computation = get >>= \old -> computation <* put old

-- * Pretty printer
--
-- These functions are in core to avoid circular dependency between core and
-- pretty printer module.
arrow, dot, lambda, lett :: Doc
arrow = char '→'
lambda = char 'λ'
dot = char '.'
lett = text "let"

class D a where
    p :: a -> Doc

instance D Ref where
    p (Ref n i t Local)  = char '%' <> text (toS n) <> int i <> colon <> p t
    p (Ref n i t Global) = char '@' <> text (toS n) <> int i <> colon <> p t

-- [TODO] - Fix type pretty printer for higher order functions
instance D Ty where
    p TUnit          = "∅"
    p TInt           = "i"
    p TBool          = "b"
    p (TArrow ta tb) = p ta <> arrow <> p tb

instance D Literal where
    p (Number n)   = int n
    p (Bool True)  = "#t"
    p (Bool False) = "#t"

instance D Core where
    p (Lit a)       = p a
    p (Var ref)     = p ref
    p (Lam r a _)   = lambda <> p r <> dot <> p a
    p (App f e)     = p f <+> p e
    p (Let var val) = p var <+> equals <+> p val

instance D a => D [a] where
    p xs = vcat $ map p xs
