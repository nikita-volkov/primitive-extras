module PrimitiveExtras.SparseSmallArray
(
  SparseSmallArray,
  empty,
  singleton,
  pair,
  insert,
  replace,
  lookup,
  elementsUnfold,
  elementsUnfoldM,
)
where

import PrimitiveExtras.Prelude hiding (lookup, empty, insert)
import PrimitiveExtras.Types
import qualified PrimitiveExtras.Bitmap as Bitmap
import qualified PrimitiveExtras.SmallArray as SmallArray


{-# INLINE empty #-}
empty :: SparseSmallArray e
empty = SparseSmallArray Bitmap.empty SmallArray.empty

-- |
-- An array with a single element at the specified index.
{-# INLINE singleton #-}
singleton :: Int -> e -> SparseSmallArray e
singleton i e = 
  let b = Bitmap.singleton i
      a = runST $ newSmallArray 1 e >>= unsafeFreezeSmallArray
      in SparseSmallArray b a

{-# INLINE pair #-}
pair :: Int -> e -> Int -> e -> SparseSmallArray e
pair i1 e1 i2 e2 =
  {-# SCC "pair" #-} 
  SparseSmallArray bitmap array
  where 
    bitmap = Bitmap.pair i1 i2
    array = SmallArray.orderedPair i1 e1 i2 e2

{-|
Insert an element value at the index.
It's your obligation to ensure that the index is empty before the operation.
-}
{-# INLINE insert #-}
insert :: Int -> e -> SparseSmallArray e -> SparseSmallArray e
insert i e (SparseSmallArray b a) =
  {-# SCC "insert" #-} 
  let
    sparseIndex = Bitmap.populatedIndex i b
    in SparseSmallArray (Bitmap.insert i b) (SmallArray.insert sparseIndex e a)
    
{-# INLINE replace #-}
replace :: Int -> e -> SparseSmallArray e -> SparseSmallArray e
replace i e (SparseSmallArray b a) =
  {-# SCC "replace" #-} 
  let
    sparseIndex = Bitmap.populatedIndex i b
    in SparseSmallArray b (SmallArray.set sparseIndex e a)

-- |
-- Lookup an item at the index.
{-# INLINE lookup #-}
lookup :: Int -> SparseSmallArray e -> Maybe e
lookup i (SparseSmallArray b a) =
  {-# SCC "lookup" #-} 
  if Bitmap.isPopulated i b
    then Just (indexSmallArray a (Bitmap.populatedIndex i b))
    else Nothing

{-# INLINE elementsUnfold #-}
elementsUnfold :: SparseSmallArray e -> Unfold e
elementsUnfold (SparseSmallArray _ array) = Unfold (\ f z -> foldl' f z array)

{-# INLINE elementsUnfoldM #-}
elementsUnfoldM :: Monad m => SparseSmallArray a -> UnfoldM m a
elementsUnfoldM (SparseSmallArray _ array) = SmallArray.elementsUnfoldM array
