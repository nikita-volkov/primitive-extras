module PrimitiveExtras.Pure
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified Data.Vector.Unboxed as A
import qualified Data.Vector.Primitive as B


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

primArrayPrimitiveVector :: Prim a => PrimArray a -> B.Vector a
primArrayPrimitiveVector primArray =
  B.Vector 0 (sizeofPrimArray primArray) (primArrayByteArray primArray)

primArrayUnboxedVector :: Prim a => PrimArray a -> A.Vector a
primArrayUnboxedVector primArray =
  unsafeCoerce (primArrayPrimitiveVector primArray)

{-# INLINE findInSmallArray #-}
findInSmallArray :: (a -> Bool) -> SmallArray a -> Maybe a
findInSmallArray test array =
  {-# SCC "findInSmallArray" #-} 
  let
    !size = sizeofSmallArray array
    iterate index = if index < size
      then let
        element = indexSmallArray array index
        in if test element
          then Just element
          else iterate (succ index)
      else Nothing
    in iterate 0

{-# INLINE findWithIndexInSmallArray #-}
findWithIndexInSmallArray :: (a -> Bool) -> SmallArray a -> Maybe (Int, a)
findWithIndexInSmallArray test array =
  {-# SCC "findWithIndexInSmallArray" #-} 
  let
    !size = sizeofSmallArray array
    iterate index = if index < size
      then let
        element = indexSmallArray array index
        in if test element
          then Just (index, element)
          else iterate (succ index)
      else Nothing
    in iterate 0

{-# INLINE emptySmallArray #-}
emptySmallArray :: SmallArray a
emptySmallArray = runSmallArray (newSmallArray 0 undefined)

-- |
-- Remove an element.
{-# INLINE unsetSmallArray #-}
unsetSmallArray :: Int -> SmallArray a -> SmallArray a
unsetSmallArray index array =
  {-# SCC "unset" #-}
  let size = sizeofSmallArray array
      newSize = pred size
      amountOfFollowingElements = newSize - index
      in runSmallArray $ do
        newMa <- newSmallArray newSize undefined
        copySmallArray newMa 0 array 0 index
        copySmallArray newMa index array (succ index) amountOfFollowingElements
        return newMa

{-# INLINE setSmallArray #-}
setSmallArray :: Int -> a -> SmallArray a -> SmallArray a
setSmallArray index a array =
  {-# SCC "setSmallArray" #-} 
  let
    size = sizeofSmallArray array
    in runSmallArray $ do
      newMa <- newSmallArray size undefined
      copySmallArray newMa 0 array 0 size
      writeSmallArray newMa index a
      return newMa

{-# INLINE insertSmallArray #-}
insertSmallArray :: Int -> a -> SmallArray a -> SmallArray a
insertSmallArray index a array =
  {-# SCC "insertSmallArray" #-} 
  let
    size = sizeofSmallArray array
    newSize = sizeofSmallArray array
    nextIndex = succ index
    amountOfFollowingElements = size - index
    in runSmallArray $ do
      newMa <- newSmallArray newSize a
      copySmallArray newMa 0 array 0 index
      copySmallArray newMa nextIndex array index amountOfFollowingElements
      return newMa

{-# INLINE consSmallArray #-}
consSmallArray :: a -> SmallArray a -> SmallArray a
consSmallArray a array =
  {-# SCC "consSmallArray" #-} 
  let
    size = sizeofSmallArray array
    newSize = succ size
    in runSmallArray $ do
      newMa <- newSmallArray newSize a
      copySmallArray newMa 1 array 0 size
      return newMa

{-# INLINE orderedPairSmallArray #-}
orderedPairSmallArray :: Int -> e -> Int -> e -> SmallArray e
orderedPairSmallArray i1 e1 i2 e2 =
  {-# SCC "orderedPairSmallArray" #-} 
  runSmallArray $ if 
    | i1 < i2 -> do
      a <- newSmallArray 2 e1
      writeSmallArray a 1 e2
      return a
    | i1 > i2 -> do
      a <- newSmallArray 2 e1
      writeSmallArray a 0 e2
      return a
    | otherwise -> do
      a <- newSmallArray 1 e2
      return a
