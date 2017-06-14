{-|
Module      : Olifant.Calculus
Description : The frontend of the language
-}
module Olifant.Calculus where

import Protolude

data Calculus =
    Var Text
  | Number Int
  | Bool Bool
  | App Calculus Calculus
  | Lam Text Calculus
  | Let Text Calculus
  deriving (Eq, Show)
