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
