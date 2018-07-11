module PrimitiveExtras.Folds
where

import PrimitiveExtras.Prelude hiding (fold, foldM)
import PrimitiveExtras.Types
import Control.Foldl
import qualified PrimitiveExtras.UnliftedArray as UA


unsafeDupableIO :: (state -> input -> IO state) -> IO state -> (state -> IO output) -> Fold input output
unsafeDupableIO stepInIO initInIO extractInIO =
  Fold
    (\ !state input -> unsafeDupablePerformIO (stepInIO state input))
    (unsafeDupablePerformIO initInIO)
    (\ state -> let !output = unsafeDupablePerformIO (extractInIO state) in output)

foldMInUnsafeDupableIO :: FoldM IO input output -> Fold input output
foldMInUnsafeDupableIO (FoldM step init extract) = unsafeDupableIO step init extract

indexCounts :: (Integral count, Prim count) => Int {-^ Array size -} -> Fold Int (PrimArray count)
indexCounts size = unsafeDupableIO step init extract where
  init = unsafeThawPrimArray (replicatePrimArray size 0)
  step mutable i = do
    count <- readPrimArray mutable i
    writePrimArray mutable i (succ count)
    return mutable
  extract = unsafeFreezePrimArray

{-|
This function is partial in the sense that it expects the
index vector of produced elements to be within the specified amount.
-}
unliftedArray :: PrimUnlifted element => Int {-^ Size of the array -} -> Fold (Int, element) (UnliftedArray element)
unliftedArray size =
  unsafeDupableIO step init extract
  where
    step mutable (index, element) =
      writeUnliftedArray mutable index element $> mutable
    init =
      unsafeNewUnliftedArray size
    extract =
      unsafeFreezeUnliftedArray

{-|
Having a priorly computed array of inner dimension sizes,
e.g., using the 'indexCounts' fold,
construct a fold over indexed elements into a multi-array of elements.
-}
multiPrimArray :: forall size element. (Integral size, Prim size, Prim element) => PrimArray size -> Fold (Int, element) (MultiPrimArray element)
multiPrimArray sizeArray =
  unsafeDupableIO step init extract
  where
    outerLength = sizeofPrimArray sizeArray
    init =
      Product2 <$> initIndexArray <*> initMultiArray
      where
        initIndexArray :: IO (MutablePrimArray RealWorld size)
        initIndexArray =
          unsafeThawPrimArray (replicatePrimArray outerLength 0)
        initMultiArray :: IO (UnliftedArray (MutablePrimArray RealWorld element))
        initMultiArray =
          UA.generate outerLength $ \ index -> do
            newPrimArray (fromIntegral (indexPrimArray sizeArray index))
    step (Product2 indexArray multiArray) (outerIndex, element) = do
      innerArray <- indexUnliftedArrayM multiArray outerIndex
      innerIndex <- readPrimArray indexArray outerIndex
      writePrimArray indexArray outerIndex (succ innerIndex)
      writePrimArray innerArray (fromIntegral innerIndex) element
      return (Product2 indexArray multiArray)
    extract (Product2 _ multiArray) = do
      copied <- unsafeNewUnliftedArray outerLength
      forMFromZero_ outerLength $ \ outerIndex -> do
        let mutableInnerArray = indexUnliftedArray multiArray outerIndex
        frozenInnerArray <- unsafeFreezePrimArray mutableInnerArray
        writeUnliftedArray copied outerIndex frozenInnerArray
      result <- unsafeFreezeUnliftedArray copied
      return $ MultiPrimArray $ result
