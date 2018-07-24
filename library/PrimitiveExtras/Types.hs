module PrimitiveExtras.Types
where

import PrimitiveExtras.Prelude


newtype PrimMultiArray a = PrimMultiArray (UnliftedArray (PrimArray a))

newtype TVarArray a = TVarArray (UnliftedArray (TVar a))

{-|
An immutable space-efficient sparse array, 
which can only store not more than 32 or 64 elements depending on the system architecure.
-}
data SparseSmallArray e = SparseSmallArray !Bitmap !(SmallArray e)

{-|
A word-size set of ints.
-}
newtype Bitmap = Bitmap Int
