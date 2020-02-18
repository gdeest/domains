module Domains.Reify where

import Data.Proxy
import Domains.Constraints
import Domains.Expr
import Domains.Vec
import GHC.TypeLits

-- | Reification of bounded indices.
class ReifyIx (ix :: Ix n) where
  reifyIx :: Ix n

instance ReifyIx IZ where
  reifyIx = IZ

instance ReifyIx ix => ReifyIx (IS ix) where
  reifyIx = IS (reifyIx @_ @ix)

-- | Reification of expressions.
class ReifyExpr (expr :: Expr Nat n) where
  reifyExpr :: Expr Integer n

instance KnownNat n => ReifyExpr (C n) where
  reifyExpr = C $ natVal $ Proxy @n

instance ReifyIx ix => ReifyExpr (I ix) where
  reifyExpr = I $ reifyIx @_ @ix

instance (ReifyExpr e1, ReifyExpr e2) => ReifyExpr (e1 :+ e2) where
  reifyExpr = reifyExpr @_ @e1 :+ reifyExpr @_ @e2

instance (KnownNat n, ReifyExpr e) => ReifyExpr (n :* e) where
  reifyExpr = natVal (Proxy @n) :* reifyExpr @_ @e

instance (KnownNat n, ReifyExpr e) => ReifyExpr (e :% n) where
  reifyExpr = reifyExpr @_ @e :% natVal (Proxy @n)

-- | Reification of mappings.
class ReifyMap (v :: Vec' (Expr Nat m) n) where
  reifyMap :: Vec' (Expr Integer m) n

instance ReifyMap VNil where
  reifyMap = VNil

instance (ReifyExpr e, ReifyMap es) => ReifyMap (e :- es) where
  reifyMap = reifyExpr @_ @e :- reifyMap @_ @_ @es

-- | Reification of constraints.
class ReifyConstraints (constraint :: Constraints Nat n) where
  reifyConstraints :: Constraints Integer n

instance (ReifyExpr e1, ReifyExpr e2) => ReifyConstraints (e1 :<= e2) where
  reifyConstraints = reifyExpr @_ @e1 :<= reifyExpr @_ @e2

instance (ReifyConstraints c1, ReifyConstraints c2) =>
    ReifyConstraints (c1 :&& c2) where
  reifyConstraints = reifyConstraints @_ @c1 :&& reifyConstraints @_ @c2

instance (ReifyMap v, ReifyConstraints cs) => ReifyConstraints (Map v cs) where
  reifyConstraints = Map (reifyMap @_ @_ @v) (reifyConstraints @_ @cs)
