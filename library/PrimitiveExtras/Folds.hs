module PrimitiveExtras.Folds
where

import PrimitiveExtras.Prelude hiding (fold, foldM)
import PrimitiveExtras.Types
import Control.Foldl
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified PrimitiveExtras.UnliftedArray as UA


unsafeDupableIO :: (state -> input -> IO state) -> IO state -> (state -> IO output) -> Fold input output
unsafeDupableIO stepInIO initInIO extractInIO =
  Fold
    (\ !state input -> unsafeDupablePerformIO (stepInIO state input))
    (unsafeDupablePerformIO initInIO)
    (\ state -> let !output = unsafeDupablePerformIO (extractInIO state) in output)

foldMInUnsafeDupableIO :: FoldM IO input output -> Fold input output
foldMInUnsafeDupableIO (FoldM step init extract) = unsafeDupableIO step init extract

intCounts :: Int {-^ Vector size -} -> Fold Int {-^ Index -} (UV.Vector Word32)
intCounts size = unsafeDupableIO step init extract where
  step mutable i = MUV.modify mutable succ i $> mutable
  init = MUV.replicate size 0
  extract = UV.unsafeFreeze

{-|
This function is partial in the sense that it expects the
indexVec of produced elements to be within the specified amount.
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

multiPrimArray :: Prim value => UV.Vector Word32 -> Fold (Int, value) (MultiPrimArray value)
multiPrimArray sizeVec =
  unsafeDupableIO step init extract
  where
    outerLength = UV.length sizeVec
    init =
      Product2 <$> initIndexVec <*> initMultiArray
      where
        initIndexVec :: IO (MUV.MVector RealWorld Word32)
        initIndexVec =
          MUV.replicate outerLength 0
        initMultiArray :: Prim value => IO (UnliftedArray (MutablePrimArray RealWorld value))
        initMultiArray =
          UA.generate outerLength $ \ index -> do
            length <- UV.unsafeIndexM sizeVec index
            newPrimArray (fromIntegral length)
    step (Product2 indexVec multiArray) (outerIndex, value) = do
      innerArray <- indexUnliftedArrayM multiArray outerIndex
      innerIndex <- MUV.unsafeRead indexVec outerIndex
      writePrimArray innerArray (fromIntegral innerIndex) value
      MUV.write indexVec outerIndex (succ innerIndex)
      return (Product2 indexVec multiArray)
    extract (Product2 _ multiArray) = do
      copied <- unsafeNewUnliftedArray outerLength
      for_ ([0..outerLength - 1] :: [Int]) $ \i -> do
        let mutable = indexUnliftedArray multiArray i
        writeUnliftedArray copied i =<< unsafeFreezePrimArray mutable
      result <- unsafeFreezeUnliftedArray copied
      return $ MultiPrimArray $ result
