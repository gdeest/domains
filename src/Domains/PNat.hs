module Domains.PNat where

-- | Peano natural numbers.
data PNat = Z | S PNat
  deriving (Show, Eq)

-- Type synonyms for low Peano numbers.
type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4

