module PrimitiveExtras.Instances
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types

deriving instance (Eq a, Prim a) => Eq (PrimMultiArray a)

deriving instance (Ord a, Prim a) => Ord (PrimMultiArray a)

instance (Show a, Prim a) => Show (PrimMultiArray a) where
  show (PrimMultiArray outerArray) =
    unliftedArrayToList outerArray &
    map primArrayToList &
    show
