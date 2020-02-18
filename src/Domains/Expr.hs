module Domains.Expr where

import Data.Proxy
import Domains.PNat
import Domains.Vec
import GHC.TypeLits

data Expr t (n :: PNat) where
  -- | Constant expression.
  C :: t -> Expr t n
  -- | Value of i-th coordinate.
  I :: Ix n -> Expr t n
  -- | Sum.
  (:+) :: Expr t n -> Expr t n -> Expr t n
  -- | Product by a constant.
  (:*) :: t -> Expr t n -> Expr t n
  -- | Constant modulo.
  (:%) :: Expr t n -> t -> Expr t n


deriving instance Show (Expr Integer n)

infixr 6 :*
infixr 6 :%
infixr 5 :+

type X0 = I I0
type X1 = I I1
type X2 = I I2
type X3 = I I3
type X4 = I I4
type X5 = I I5
