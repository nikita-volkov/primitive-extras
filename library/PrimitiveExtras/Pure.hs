module PrimitiveExtras.Pure
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as A


{-| Get length of the outer dimension of a primitive multi array -}
primMultiArrayOuterLength :: PrimMultiArray a -> Int
primMultiArrayOuterLength (PrimMultiArray outerDimension) = sizeofUnliftedArray outerDimension
