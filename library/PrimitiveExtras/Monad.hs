module PrimitiveExtras.Monad
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.Fold as A


{-| Given a size of the outer array and a function, which executes a fold over indexed elements in a monad,
constructs a multi prim array -}
multiPrimArray :: (Monad m, Prim element) => Int -> (forall x. Fold (Int, element) x -> m x) -> m (MultiPrimArray element)
multiPrimArray outerArraySize runFold =
  do
    indexCounts <- runFold (lmap fst (A.indexCounts outerArraySize))
    runFold (A.multiPrimArray (indexCounts :: PrimArray Word32))
