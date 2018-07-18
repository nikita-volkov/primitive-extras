module PrimitiveExtras.Types
where

import PrimitiveExtras.Prelude


newtype PrimMultiArray a = PrimMultiArray (UnliftedArray (PrimArray a))

newtype TVarArray a = TVarArray (UnliftedArray (TVar a))
