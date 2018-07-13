module PrimitiveExtras.Cereal.Put
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import Data.Serialize.Put
import qualified PrimitiveExtras.Monad as Monad


primArray :: Prim element => Putter element -> Putter (PrimArray element)
primArray element primArrayValue =
  size <> elements
  where
    size = putInt64le (fromIntegral (sizeofPrimArray primArrayValue))
    elements = traversePrimArray_ element primArrayValue

primMultiArray :: Prim element => Putter element -> Putter (PrimMultiArray element)
primMultiArray element (PrimMultiArray outerArrayValue) =
  size <> innerArrays
  where
    size = putInt64le (fromIntegral (sizeofUnliftedArray outerArrayValue))
    innerArrays = Monad.traverseUnliftedArray_ (primArray element) outerArrayValue
