module Domains.Constraints where

import Domains.Expr
import Domains.PNat
import Domains.Vec

data Constraints t (n :: PNat) where
  (:<=) :: Expr t n -> Expr t n -> Constraints t n
  (:&&) :: Constraints t n -> Constraints t n -> Constraints t n
  Neg :: Constraints t n -> Constraints t n
  Map :: Vec' (Expr t m) n -> Constraints t m -> Constraints t n

infixr 3 :&&
infixr 4 :<=

type (:||) e1 e2 = Neg ((Neg e1) :&& (Neg e2))
infixr 3 :||
type (:==) e1 e2 = e1 :<= e2 :&& e2 :<= e1
infixr 4 :==

instance Show (Constraints Integer n) where
  -- | TODO: Use showsPrec instead to remove some parentheses.
  show (e1 :<= e2) = concat
    [ "(", show e1, ")"
    , " :<= "
    , "(", show e2, ")" ]

  show (e1 :&& e2) = concat
    [ "(", show e1, ")"
    , " :&& "
    , "(", show e2, ")" ]

  show (Neg cs) = concat
    [ "Neg (", show cs, ")" ]

  show (Map vs cs) = concat
    [ "Map (", show vs, ") (", show cs, ")" ]
