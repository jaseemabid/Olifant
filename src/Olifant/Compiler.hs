{-|
Module      : Olifant.Compiler
Description : Compile Calculus to Core
-}
module Olifant.Compiler where

import Protolude hiding (cast)
import qualified Data.Set as Set

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

-- | Find undefined variables
--
-- This one can serve as an example for a nano pass!
--
undef :: CoreUT -> Either Text CoreUT
undef core = evalState (undef' core) Set.empty
  where
    undef' :: CoreUT -> State (Set Text) (Either Text CoreUT)
    undef' e@(Var (Ref a _t)) = do
      x <- Set.member a <$> get
      return $ if x then Right e else Left a

    undef' (l@Lit{} ) = return $ Right l

    undef' (App _f exp') = undef' exp'

    -- [todo] - Evaluate body with arg as well
    undef' (Lam _fn arg' body') = undef' (Var arg') >> undef' body'

    undef' (Let _var val) =  undef' val
