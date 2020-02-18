module Domains.Vec where

import Domains.PNat

import GHC.TypeLits (Nat)

-- | Length-indexed vectors.
data Vec' t (n :: PNat) where
  VNil :: Vec' t Z
  (:-) :: t -> Vec' t n -> Vec' t (S n)

infixr 2 :-

deriving instance Show t => Show (Vec' t n)

type Vec = Vec' Integer
type VecS = Vec' Nat

-- | Bounded indices.
data Ix (n :: PNat) where
  IZ :: Ix n
  IS :: Ix n -> Ix (S n)

deriving instance Show (Ix n)

-- Type synonyms for low indices.
type I0 = IZ
type I1 = IS I0
type I2 = IS I1
type I3 = IS I2
type I4 = IS I3
type I5 = IS I4
