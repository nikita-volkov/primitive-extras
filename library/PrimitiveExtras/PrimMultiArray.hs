module PrimitiveExtras.PrimMultiArray
(
  PrimMultiArray,
  create,
  replicateM,
  outerLength,
  toAssocsUnfold,
  toIndicesUnfold,
  toUnfoldAt,
  toAssocsUnfoldM,
  toIndicesUnfoldM,
  toUnfoldAtM,
  cerealGet,
  cerealPut,
)
where

import PrimitiveExtras.Prelude hiding (replicateM)
import PrimitiveExtras.Types
import qualified DeferredFolds.Unfold as Unfold
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified PrimitiveExtras.UnliftedArray as UnliftedArray
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.Folds as Folds
import qualified Data.Serialize as Cereal


deriving instance (Eq a, Prim a) => Eq (PrimMultiArray a)

deriving instance (Ord a, Prim a) => Ord (PrimMultiArray a)

instance (Show a, Prim a) => Show (PrimMultiArray a) where
  show (PrimMultiArray outerArray) =
    unliftedArrayToList outerArray &
    map primArrayToList &
    show

{-| Given a size of the outer array and a function, which executes a fold over indexed elements in a monad,
constructs a prim multi-array -}
create :: (Monad m, Prim element) => Int -> (forall x. Fold (Int, element) x -> m x) -> m (PrimMultiArray element)
create outerArraySize runFold =
  do
    indexCounts <- runFold (lmap fst (Folds.indexCounts outerArraySize))
    runFold (Folds.primMultiArray (indexCounts :: PrimArray Word32))

replicateM :: (Monad m, Prim a) => Int -> m (PrimArray a) -> m (PrimMultiArray a)
replicateM size elementM =
  do
    !mutable <- return (unsafeDupablePerformIO (unsafeNewUnliftedArray size))
    let 
      iterate index =
        if index < size
          then do
            element <- elementM
            let !() = unsafeDupablePerformIO (writeUnliftedArray mutable index element)
            iterate (succ index)
          else return (PrimMultiArray (unsafePerformIO (unsafeFreezeUnliftedArray mutable)))
      in iterate 0

{-| Get length of the outer dimension of a primitive multi array -}
outerLength :: PrimMultiArray a -> Int
outerLength (PrimMultiArray outerDimension) = sizeofUnliftedArray outerDimension

toAssocsUnfold :: Prim a => PrimMultiArray a -> Unfold (Int, a)
toAssocsUnfold = Unfold.unfoldM . toAssocsUnfoldM

toIndicesUnfold :: PrimMultiArray a -> Unfold Int
toIndicesUnfold (PrimMultiArray ua) = Unfold.intsInRange 0 (pred (sizeofUnliftedArray ua))

toUnfoldAt :: Prim prim => PrimMultiArray prim -> Int -> Unfold prim
toUnfoldAt (PrimMultiArray ua) index = UnliftedArray.at ua index empty PrimArray.toElementsUnfold

toAssocsUnfoldM :: (Monad m, Prim a) => PrimMultiArray a -> UnfoldM m (Int, a)
toAssocsUnfoldM pma =
  do
    index <- toIndicesUnfoldM pma
    element <- toUnfoldAtM pma index
    return (index, element)

toIndicesUnfoldM :: Monad m => PrimMultiArray a -> UnfoldM m Int
toIndicesUnfoldM (PrimMultiArray ua) = UnfoldM.intsInRange 0 (pred (sizeofUnliftedArray ua))

toUnfoldAtM :: (Monad m, Prim prim) => PrimMultiArray prim -> Int -> UnfoldM m prim
toUnfoldAtM (PrimMultiArray ua) index = UnliftedArray.at ua index empty PrimArray.toElementsUnfoldM

cerealGet :: Prim element => Cereal.Get Int -> Cereal.Get element -> Cereal.Get (PrimMultiArray element)
cerealGet int element =
  do
    size <- int
    replicateM size (PrimArray.cerealGet int element)

cerealPut :: Prim element => Cereal.Putter Int -> Cereal.Putter element -> Cereal.Putter (PrimMultiArray element)
cerealPut int element (PrimMultiArray outerArrayValue) =
  size <> innerArrays
  where
    size = int (sizeofUnliftedArray outerArrayValue)
    innerArrays = UnliftedArray.traverse_ (PrimArray.cerealPut int element) outerArrayValue
