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
