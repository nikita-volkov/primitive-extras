module PrimitiveExtras.Unfolds
where

import PrimitiveExtras.Prelude hiding (fold)
import PrimitiveExtras.Types
import DeferredFolds.Unfold
import qualified PrimitiveExtras.Folds as A
import qualified PrimitiveExtras.UnliftedArray as B


multiPrimArrayIndices :: MultiPrimArray a -> Unfold Int
multiPrimArrayIndices (MultiPrimArray ua) =
  intsInRange 0 (pred (sizeofUnliftedArray ua))

multiPrimArrayAt :: Prim prim => MultiPrimArray prim -> Int -> Unfold prim
multiPrimArrayAt (MultiPrimArray ua) index =
  B.at ua index empty primArrayPrims

primArrayPrims :: Prim prim => PrimArray prim -> Unfold prim
primArrayPrims ba = Unfold $ \f z -> foldlPrimArray' f z ba
