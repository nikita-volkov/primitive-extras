module PrimitiveExtras.UnliftedArray.Potoki.Consume
where

import PrimitiveExtras.Prelude
import Potoki.Core.Consume
import qualified Potoki.Core.Fetch as A


{-|
This function is partial in the sense that it expects the
amount of produced elements to match the specified amount.
-}
sizedUnsafe :: PrimUnlifted element => Int -> Consume (Int, element) (UnliftedArray element)
sizedUnsafe size =
  Consume $ \ (A.Fetch fetchIO) -> do
    mutable <- unsafeNewUnliftedArray size
    let
      iterate = do
        mayP <- fetchIO
        case mayP of
          Nothing -> unsafeFreezeUnliftedArray mutable
          Just (index, element) -> do
            writeUnliftedArray mutable index element
            iterate
      in iterate
