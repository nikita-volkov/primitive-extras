module PrimitiveExtras.Cereal.Get
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import Data.Serialize.Get
import PrimitiveExtras.Monad


primArray :: Prim element => Get element -> Get (PrimArray element)
primArray element =
  do
    size <- fromIntegral <$> getInt64le
    replicateMPrimArray size element

primMultiArray :: Prim element => Get element -> Get (PrimMultiArray element)
primMultiArray element =
  do
    size <- fromIntegral <$> getInt64le
    replicateMMultiPrimArray size (primArray element)
