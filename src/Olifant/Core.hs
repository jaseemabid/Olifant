module Olifant.Core where

import Protolude

-- | Simple calculus
data Calc
    = Number Integer
    | Plus Calc Calc
    -- ^ Classic lambda calculus
    | Var Text
    | App Text Calc
    | Lam Text Text Calc
    deriving (Read, Show)
