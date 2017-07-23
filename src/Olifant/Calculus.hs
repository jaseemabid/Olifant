{-|
Module      : Olifant.Calculus
Description : The frontend of the language
-}
module Olifant.Calculus where

import Protolude

import Olifant.Core (Ty)

data Calculus
    = Var Text
    | Number Int
    | Bool Bool
    | App Calculus
          [Calculus]
    | Lam [(Ty, Text)]
          Calculus
    | Let Text
          Calculus
    deriving (Eq, Show)
