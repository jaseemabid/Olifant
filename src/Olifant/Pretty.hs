-----------------------------------------------------------------------------
-- |
-- Module      :  Olifant.Pretty
-- Description :  Pretty print AST
--
-----------------------------------------------------------------------------
module Olifant.Pretty where

import Protolude hiding ((<>))

import Olifant.Core
import Data.Foldable (foldr1)
import Text.PrettyPrint

arrow :: Doc
arrow = text "->"

pt :: Tipe -> Doc
pt TInt = "i"
pt TBool = "b"
pt (TArrow ts) = foldr1 (\a b -> a <+> arrow <+> b) $ map pt ts

pu :: CoreUT -> Doc
pu (Var _ (Ref n)) = text $ toS n
pu (Lit _ (LNumber n)) = int n
pu (Lit _ (LBool True)) = "#t"
pu (Lit _ (LBool False)) = "#t"
pu (App _ a b) = pu a <+> pu b
pu (Lam _ (Ref a) b) = char 'λ' <> text (toS a) <> char '.' <+> pu b

p :: Core -> Doc
p (Var t (Ref n)) = text (toS n) <+> "::" <+> pt t
p (Lit _ (LNumber n)) = int n
p (Lit _ (LBool True)) = "#t"
p (Lit _ (LBool False)) = "#t"
p (App t a b) = p a <+> p b <+> "::" <+> pt t
p (Lam t (Ref a) b) = char 'λ' <> text (toS a) <> pt t <> char '.' <+> p b
