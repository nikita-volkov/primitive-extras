module PrimitiveExtras.PrimArray
where

import PrimitiveExtras.Prelude hiding (replicateM, traverse_)
import PrimitiveExtras.Types
import qualified Data.Serialize as Cereal
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Primitive as PrimitiveVector
import qualified PrimitiveExtras.Folds as Folds
import qualified PrimitiveExtras.FoldMs as FoldMs
import qualified Data.ByteString.Short.Internal as ShortByteString


oneHot :: Prim a => Int {-^ Size -} -> Int {-^ Index -} -> a -> PrimArray a
oneHot size index value =
  runST $ do
    marr <- newPrimArray size
    writePrimArray marr index value
    unsafeFreezePrimArray marr

generate :: Prim a => Int -> (Int -> IO a) -> IO (PrimArray a)
generate size elementIO =
  do
    array <- newPrimArray size
    let
      loop index =
        if index < size
          then do
            element <- elementIO index
            writePrimArray array index element
            loop (succ index)
          else unsafeFreezePrimArray array
      in loop 0

replicate :: Prim a => Int -> IO a -> IO (PrimArray a)
replicate size elementIO =
  do
    array <- newPrimArray size
    let
      loop index =
        if index < size
          then do
            element <- elementIO
            writePrimArray array index element
            loop (succ index)
          else unsafeFreezePrimArray array
      in loop 0

{-| Please notice that this function is highly untested -}
replicateM :: (Monad m, Prim element) => Int -> m element -> m (PrimArray element)
replicateM size elementM =
  do
    !mutable <- return (unsafeDupablePerformIO (newPrimArray size))
    let 
      iterate index =
        if index < size
          then do
            element <- elementM
            let !() = unsafeDupablePerformIO (writePrimArray mutable index element)
            iterate (succ index)
          else return (unsafePerformIO (unsafeFreezePrimArray mutable))
      in iterate 0

traverse_ = traversePrimArray_

traverseWithIndexInRange_ :: Prim a => PrimArray a -> Int -> Int -> (Int -> a -> IO ()) -> IO ()
traverseWithIndexInRange_ primArray from to action =
  let iterate index = if index < to
        then do
          action index $! indexPrimArray primArray index
          iterate (succ index)
        else return ()
      in iterate from

toElementsUnfoldl :: Prim prim => PrimArray prim -> Unfoldl prim
toElementsUnfoldl ba = Unfoldl $ \f z -> foldlPrimArray' f z ba

toElementsUnfoldlM :: (Monad m, Prim prim) => PrimArray prim -> UnfoldlM m prim
toElementsUnfoldlM ba = UnfoldlM $ \f z -> foldlPrimArrayM' f z ba

toByteArray :: PrimArray a -> ByteArray
toByteArray (PrimArray unliftedByteArray) =
  ByteArray unliftedByteArray

toPrimitiveVector :: Prim a => PrimArray a -> PrimitiveVector.Vector a
toPrimitiveVector primArray =
  PrimitiveVector.Vector 0 (sizeofPrimArray primArray) (toByteArray primArray)

toUnboxedVector :: Prim a => PrimArray a -> UnboxedVector.Vector a
toUnboxedVector primArray =
  unsafeCoerce (toPrimitiveVector primArray)

cerealGet :: Prim element => Cereal.Get Int -> Cereal.Get element -> Cereal.Get (PrimArray element)
cerealGet int element =
  do
    size <- int
    replicateM size element

cerealGetAsInMemory :: Prim element => Cereal.Get Int -> Cereal.Get (PrimArray element)
cerealGetAsInMemory int =
  do
    size <- int
    ShortByteString.SBS ba <- Cereal.getShortByteString size
    return (PrimArray ba)

cerealPut :: Prim element => Cereal.Putter Int -> Cereal.Putter element -> Cereal.Putter (PrimArray element)
cerealPut int element primArrayValue =
  size <> elements
  where
    size = int (sizeofPrimArray primArrayValue)
    elements = traverse_ element primArrayValue

cerealPutAsInMemory :: Prim element => Cereal.Putter Int -> Cereal.Putter (PrimArray element)
cerealPutAsInMemory int primArrayValue@(PrimArray ba) =
  size <> elements
  where
    size = int (sizeofByteArray (ByteArray ba))
    elements = Cereal.putShortByteString (ShortByteString.SBS ba)

{-|
Given a size of the array,
construct a fold, which produces an array of index counts.
-}
indexCountsFold :: (Integral count, Prim count) => Int {-^ Array size -} -> Fold Int (PrimArray count)
indexCountsFold = Folds.indexCounts

{-|
Given a size of the array,
construct a fold, which produces an array of elements.
-}
elementsFoldM :: Prim a => Int {-^ Array size -} -> FoldM IO a (PrimArray a)
elementsFoldM = FoldMs.primArray
