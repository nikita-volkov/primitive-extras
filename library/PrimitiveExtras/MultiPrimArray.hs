module PrimitiveExtras.MultiPrimArray
(
  MultiPrimArray,
  -- * Constructors
  constructFolding,
  -- * Unfolds
  indicesUnfold,
)
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.Fold as A
import qualified PrimitiveExtras.Unfold as B


{-| Given a size of the outer array and a function, which executes a fold over indexed elements in a monad,
constructs a multi prim array -}
constructFolding :: (Monad m, Prim element) => Int -> (forall x. Fold (Int, element) x -> m x) -> m (MultiPrimArray element)
constructFolding outerArraySize runFold =
  do
    indexCounts <- runFold (lmap fst (A.indexCounts outerArraySize))
    runFold (A.multiPrimArray (indexCounts :: PrimArray Word32))

indicesUnfold :: MultiPrimArray element -> Unfold Int
indicesUnfold = B.multiPrimArrayIndices

atUnfold :: Prim prim => MultiPrimArray prim -> Int -> Unfold prim
atUnfold = B.multiPrimArrayAt
