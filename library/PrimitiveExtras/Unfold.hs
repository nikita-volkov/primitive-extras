module PrimitiveExtras.Unfold
where

import PrimitiveExtras.Prelude hiding (fold)
import PrimitiveExtras.Types
import DeferredFolds.Unfold
import qualified PrimitiveExtras.Fold as A
import qualified PrimitiveExtras.UnliftedArray as B
import qualified PrimitiveExtras.UnfoldM as C


primMultiArrayAssocs :: Prim a => PrimMultiArray a -> Unfold (Int, a)
primMultiArrayAssocs = unfoldM . C.primMultiArrayAssocs

primMultiArrayIndices :: PrimMultiArray a -> Unfold Int
primMultiArrayIndices (PrimMultiArray ua) =
  intsInRange 0 (pred (sizeofUnliftedArray ua))

primMultiArrayAt :: Prim prim => PrimMultiArray prim -> Int -> Unfold prim
primMultiArrayAt (PrimMultiArray ua) index =
  B.at ua index empty primArray

primArray :: Prim prim => PrimArray prim -> Unfold prim
primArray ba = Unfold $ \f z -> foldlPrimArray' f z ba
