module PrimitiveExtras.Monad
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.Fold as A


{-| Given a size of the outer array and a function, which executes a fold over indexed elements in a monad,
constructs a prim multi-array -}
primMultiArray :: (Monad m, Prim element) => Int -> (forall x. Fold (Int, element) x -> m x) -> m (PrimMultiArray element)
primMultiArray outerArraySize runFold =
  do
    indexCounts <- runFold (lmap fst (A.indexCounts outerArraySize))
    runFold (A.primMultiArray (indexCounts :: PrimArray Word32))

replicateMMultiPrimArray :: (Monad m, Prim a) => Int -> m (PrimArray a) -> m (PrimMultiArray a)
replicateMMultiPrimArray size elementM =
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

{-| Please notice that this function is highly untested -}
replicateMPrimArray :: (Monad m, Prim element) => Int -> m element -> m (PrimArray element)
replicateMPrimArray size elementM =
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

traverseUnliftedArray_ :: (Monad m, PrimUnlifted a) => (a -> m ()) -> UnliftedArray a -> m ()
traverseUnliftedArray_ action array =
  let
    size = sizeofUnliftedArray array
    iterate index = if index < size
      then do
        element <- indexUnliftedArrayM array index
        action element
        iterate (succ index)
      else return ()
    in iterate 0
