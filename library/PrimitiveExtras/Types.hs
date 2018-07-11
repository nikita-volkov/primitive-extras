module PrimitiveExtras.Types
where

import PrimitiveExtras.Prelude


newtype PrimMultiArray a = PrimMultiArray (UnliftedArray (PrimArray a))
