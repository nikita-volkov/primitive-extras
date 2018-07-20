module PrimitiveExtras.Bitmap
(
  Bitmap,
  empty,
  singleton,
  insert,
  invert,
  list,
  pair,
  populatedIndex,
  isPopulated,
  population,
  null,
  bits,
  populatedIndicesList,
  word,
  allBitsUnfold,
  populatedBitsUnfold,
  indicesAmongstPopulatedBitsUnfold,
)
where

import PrimitiveExtras.Prelude hiding (traverse_, insert, null, empty)
import PrimitiveExtras.Types
import qualified DeferredFolds.Unfold as Unfold


{-# NOINLINE maxSize #-}
maxSize :: Int
maxSize = finiteBitSize (undefined :: Int)

{-# NOINLINE maxBit #-}
maxBit :: Int
maxBit = pred maxSize

{-# NOINLINE allBitsList #-}
allBitsList :: [Int]
allBitsList = [0 .. maxBit]

-- * Constructors
-------------------------

{-# INLINE empty #-}
empty :: Bitmap
empty = Bitmap 0

{-# INLINE singleton #-}
singleton :: Int -> Bitmap
singleton = Bitmap . bit

{-# INLINE insert #-}
insert :: Int -> Bitmap -> Bitmap
insert i = Bitmap . (bit i .|.) . word

{-# INLINE invert #-}
invert :: Int -> Bitmap -> Bitmap
invert i = Bitmap . (bit i `xor`) . word

{-# INLINE list #-}
list :: [Int] -> Bitmap
list = Bitmap . foldr (.|.) 0 . map bit

{-# INLINE pair #-}
pair :: Int -> Int -> Bitmap
pair i1 i2 = Bitmap (bit i1 .|. bit i2)

-- * Accessors
-------------------------

-- |
-- A number of non-zero bits, preceding this one.
{-# INLINE populatedIndex #-}
populatedIndex :: Int -> Bitmap -> Int
populatedIndex i (Bitmap int) = popCount (int .&. (bit i - 1))

{-# INLINE isPopulated #-}
isPopulated :: Int -> Bitmap -> Bool
isPopulated index (Bitmap int) = testBit int index

{-# INLINE population #-}
population :: Bitmap -> Int
population (Bitmap int) = popCount int

{-# INLINE null #-}
null :: Bitmap -> Bool
null (Bitmap int) = int == 0

{-# INLINE bits #-}
bits :: Bitmap -> [Int]
bits (Bitmap int) = filter (testBit int) allBitsList

{-# INLINE populatedIndicesList #-}
populatedIndicesList :: Bitmap -> [Int]
populatedIndicesList = enumFromTo 0 . pred . population

{-# INLINE word #-}
word :: Bitmap -> Word
word (Bitmap word) = word

{-# NOINLINE allBitsUnfold #-}
allBitsUnfold :: Unfold Int
allBitsUnfold = Unfold.intsInRange 0 maxBit

populatedBitsUnfold :: Bitmap -> Unfold Int
populatedBitsUnfold bitmap = Unfold.filter (flip isPopulated bitmap) allBitsUnfold

indicesAmongstPopulatedBitsUnfold :: Bitmap -> Unfold Int
indicesAmongstPopulatedBitsUnfold bitmap = Unfold.intsInRange 0 (pred (population bitmap))
