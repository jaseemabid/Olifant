{-|
Module      : Olifant.Calculus
Description : The frontend of the language
-}
module Olifant.Calculus where

import Protolude

import Olifant.Core (Tipe)

data Calculus =
    Var Text
  | Number Int
  | Bool Bool
  | App Calculus Calculus
  | Lam Text Tipe Calculus
  | Let Text Calculus
  deriving (Eq, Show)
