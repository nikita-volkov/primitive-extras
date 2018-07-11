module PrimitiveExtras.IO
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as A


generateUnliftedArray :: PrimUnlifted a => Int -> (Int -> IO a) -> IO (UnliftedArray a)
generateUnliftedArray = A.generate

replicateUnliftedArray :: PrimUnlifted a => Int -> IO a -> IO (UnliftedArray a)
replicateUnliftedArray = A.replicateIO
