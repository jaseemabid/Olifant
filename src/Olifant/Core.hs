{-|
Module      : Olifant.Core
Description : Core data structures of the compiler
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Olifant.Core where

import Data.String
import Protolude        hiding ((<>))
import Text.Parsec      (ParseError)
import Text.PrettyPrint

--  - https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType
--  - http://blog.ezyang.com/2013/05/the-ast-typing-problem/

-- | Code generator treats local and global variables differently
data Scope = Local | Global
  deriving (Eq, Ord, Show)

-- | A reference; an embedded data structure avoids the need for a symbol table
data Ref = Ref {rname :: Text, rty :: Ty, scope :: Scope}
    deriving (Eq, Ord, Show)

data Literal = LNumber Int | LBool Bool
    deriving (Eq, Show)

-- [TODO] - Replace TArrow with ~>
data Ty = TUnit | TInt | TBool | TArrow Ty Ty
    deriving (Eq, Ord, Show)

-- | An annotated lambda calculus expression
--
-- The tree contains redundant information that can be used by the verifier to
-- make sure a step didn't go wrong. For example, the type parameter in both
-- `App` and `Lam` can be fetched as well as derived. They should always match,
-- and if it doesn't; something went wrong somewhere.
data Expr
    = Var Ref
    | Lit Literal
    | App Ty Expr [Expr]
    | Lam Ty [Ref] Expr
    deriving (Eq, Show)

-- | Top level binding of a lambda calc expression to a name
data Bind = Bind Ref Expr
  deriving (Eq, Show)

-- | A program is a list of bindings and an expression
data Progn = Progn [Bind] Expr
  deriving (Eq, Show)

-- * Type helpers
ty :: Expr -> Ty
ty (Var (Ref _ t _)) = t
ty (Lit (LNumber _)) = TInt
ty (Lit (LBool _))   = TBool
ty (App t _ _)       = t
ty (Lam t _ _)       = t

-- | Return type of a type
retT :: Ty -> Ty
retT (TArrow _ tb) = retT tb
retT t             = t

-- | Arguments of a type
argT :: Ty -> Ty
argT (TArrow ta _) = ta
argT t             = t

-- | Make function type out of the arguments and body
unapply :: Ty -> [Ty] -> Ty
unapply = foldr TArrow

-- | Apply a type to a function
--    > apply (TArrow [TInt, TBool]) [TInt]
--    TBool
apply :: Ty -> [Ty] -> Maybe Ty
apply t [] = Just t
apply (TArrow ta tb) (t: ts)
  | t == ta = apply tb ts
  | otherwise = Nothing
apply _ _ = Nothing

-- * Error handling and state monad

-- | Errors raised by the compiler
--
data Error =
    GenError Text
  | Panic Text
  | ParseError ParseError
  | SyntaxError Text
  | UndefinedError Ref
  | TyError -- {expr :: Expr, expected :: Ty, reality :: Ty}
  deriving (Eq, Show)

-- Olifant Monad
--
-- A `State + Error + IO` transformer with Error type fixed to `Error`
newtype Olifant s a = Olifant
    { runOlifant :: StateT s (Except Error) a
    } deriving (Applicative, Functor, Monad, MonadError Error, MonadState s)

-- | Run a computation in olifant monad with some state and return the result
evalM :: Olifant s a -> s -> Either Error a
evalM c s = runIdentity $ runExceptT $ evalStateT (runOlifant c) s

-- | Run a computation in olifant monad with some state and return new state
execM :: Olifant s a -> s -> Either Error s
execM c s = runIdentity $ runExceptT $ execStateT (runOlifant c) s

-- * Instance declarations

instance IsString Ref where
    fromString x = Ref (toS x) TUnit Local

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
instance D Ty where
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
