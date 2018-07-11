module PrimitiveExtras.Types
where

import PrimitiveExtras.Prelude


newtype MultiPrimArray a = MultiPrimArray (UnliftedArray (PrimArray a))
