module Domains
  ( module Domains.PNat
  , module Domains.Expr
  , module Domains.Vec
  , module Domains.Reify
  , module Domains.Constraints
  , Domain
  , Range
  )
where

import Domains.PNat
import Domains.Expr
import Domains.Vec
import Domains.Reify
import Domains.Constraints
import GHC.TypeLits

type Range l h e =
  C l :<= e :&&
  C 1 :+ e :<= C h

type Domain (n :: PNat) = Constraints Nat n
