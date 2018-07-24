module PrimitiveExtras.TVarArray
(
  TVarArray,
  new,
  freezeAsPrimArray,
  modifyAt,
)
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as UnliftedArray


new :: a -> Int -> IO (TVarArray a)
new a size = TVarArray <$> UnliftedArray.replicateIO size (newTVarIO a)

freezeAsPrimArray :: Prim a => TVarArray a -> IO (PrimArray a)
freezeAsPrimArray (TVarArray varArray) =
  do
    let size = sizeofUnliftedArray varArray
    mpa <- newPrimArray size
    forMFromZero_ size $ \ index -> do
      var <- indexUnliftedArrayM varArray index
      value <- atomically (readTVar var)
      writePrimArray mpa index value
    unsafeFreezePrimArray mpa

modifyAt :: TVarArray a -> Int -> (a -> a) -> IO ()
modifyAt (TVarArray array) index fn =
  do
    var <- indexUnliftedArrayM array index
    atomically $ modifyTVar' var fn
