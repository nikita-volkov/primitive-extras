module PrimitiveExtras.Unfold
where

import PrimitiveExtras.Prelude hiding (fold)
import PrimitiveExtras.Types
import DeferredFolds.Unfold
import qualified PrimitiveExtras.Fold as A
import qualified PrimitiveExtras.UnliftedArray as B


primMultiArrayIndices :: PrimMultiArray a -> Unfold Int
primMultiArrayIndices (PrimMultiArray ua) =
  intsInRange 0 (pred (sizeofUnliftedArray ua))

primMultiArrayAt :: Prim prim => PrimMultiArray prim -> Int -> Unfold prim
primMultiArrayAt (PrimMultiArray ua) index =
  B.at ua index empty primArrayPrims

primArrayPrims :: Prim prim => PrimArray prim -> Unfold prim
primArrayPrims ba = Unfold $ \f z -> foldlPrimArray' f z ba
