module Main.Gens where

import Prelude hiding (choose, index)
import Test.QuickCheck.Gen
import Focus (Focus(..))
import Main.Transaction (Transaction)
import qualified Main.Transaction as Transaction
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray


element :: Gen Int
element = choose (0, 999)

index :: Gen Int
index = choose (0, 9)

lookupTransaction :: (Show element, Eq element) => Gen (Transaction element)
lookupTransaction = Transaction.lookup <$> index

setTransaction :: Gen (Transaction Int)
setTransaction = Transaction.set <$> index <*> element

unsetTransaction :: Gen (Transaction element)
unsetTransaction = Transaction.unset <$> index

unfoldTransaction :: (Show element, Eq element) => Gen (Transaction element)
unfoldTransaction = pure Transaction.elementsUnfold

transaction :: Gen (Transaction Int)
transaction =
  frequency
    [
      (9, lookupTransaction),
      (9, setTransaction),
      (9, unsetTransaction)
    ]

maybeList :: Gen [Maybe Int]
maybeList =
  replicateM (finiteBitSize (undefined :: Int)) $ frequency $
  [
    (4, fmap Just element),
    (1, pure Nothing)
  ]

sparseSmallArray :: Gen (SparseSmallArray.SparseSmallArray Int)
sparseSmallArray =
  SparseSmallArray.maybeList <$> maybeList
