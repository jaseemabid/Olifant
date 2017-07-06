module Olifant (
    Calculus(..)
  , compile
  , gen
  , parser
  ) where

import Olifant.Gen (gen)
import Olifant.Calculus (Calculus(..))
import Olifant.Parser (parser)
import Olifant.Compiler (compile)
