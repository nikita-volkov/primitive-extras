module PrimitiveExtras.Instances
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types

deriving instance (Eq a, Prim a) => Eq (PrimMultiArray a)
