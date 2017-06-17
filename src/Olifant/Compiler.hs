{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}

module Olifant.Compiler where

import Protolude hiding (cast)

import Olifant.Core
import qualified Olifant.Calculus as C

-- | Quickly translate calculus into untyped core
--
-- This is a dumb stupid pass which is making me reconsider having the Calculus
-- type at all. Maybe I can just avoid Calculus type and parse into CoreUT
-- directly.
--
cast :: C.Calculus -> CoreUT
cast (C.Var a) = Var $ Ref a unit
cast (C.Number n) = Lit (LNumber n)
cast (C.Bool b) = Lit (LBool b)
cast (C.App fn arg') = App (cast fn) (cast arg')
cast (C.Lam n b) = Lam (Ref "~" unit) (Ref n unit) (cast b)
cast (C.Let var val) = Let (Ref var unit) (cast val)
