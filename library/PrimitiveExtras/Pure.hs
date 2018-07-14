module PrimitiveExtras.Pure
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as A


{-| Get length of the outer dimension of a primitive multi array -}
primMultiArrayOuterLength :: PrimMultiArray a -> Int
primMultiArrayOuterLength (PrimMultiArray outerDimension) = sizeofUnliftedArray outerDimension

oneHotPrimArray :: Prim a => Int {-^ Size -} -> Int {-^ Index -} -> a -> PrimArray a
oneHotPrimArray size index value =
  runST $ do
    marr <- newPrimArray size
    writePrimArray marr index value
    unsafeFreezePrimArray marr

primArrayByteArray :: PrimArray a -> ByteArray
primArrayByteArray (PrimArray unliftedByteArray) =
  ByteArray unliftedByteArray
