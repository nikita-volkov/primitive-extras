module Main.Transaction where

import Prelude
import Focus (Focus(..))
import qualified Focus
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray
import qualified Data.Text as Text
import qualified DeferredFolds.Unfoldl as Unfoldl


data Transaction element = forall result. (Show result, Eq result) => Transaction {
  name :: Text,
  applyToMaybeList :: State [Maybe element] result,
  applyToSparseSmallArray :: State (SparseSmallArray.SparseSmallArray element) result
}

instance Show (Transaction element) where
  show = Text.unpack . name

singleton :: Show element => Int -> element -> Transaction element
singleton index element =
  Transaction {
    name = "singleton " <> (fromString . show) index <> " " <> fromString (show element)
    ,
    applyToMaybeList =
      put $ map (\i' -> if index == i' then Just element else Nothing) [0 .. pred (finiteBitSize (undefined :: Int))]
    ,
    applyToSparseSmallArray =
      put $ SparseSmallArray.singleton index element
  }

set :: Show element => Int -> element -> Transaction element
set index element =
  Transaction {
    name = "set " <> (fromString . show) index <> " " <> (fromString . show) element
    ,
    applyToMaybeList = do
      l <- get
      put $ do
        (i', e') <- zip [0..] l
        return $ if index == i' then Just element else e'
    ,
    applyToSparseSmallArray = do
      ssa <- get
      case SparseSmallArray.lookup index ssa of
        Just _ -> put (SparseSmallArray.replace index element ssa)
        Nothing -> put (SparseSmallArray.insert index element ssa)
  }

unset :: Int -> Transaction element
unset index =
  Transaction {
    name = "unset " <> fromString (show index)
    ,
    applyToMaybeList = do
      l <- get
      put $ do
        (i', e') <- zip [0..] l
        return $ if index == i' then Nothing else e'
    ,
    applyToSparseSmallArray =
      get >>= put . (SparseSmallArray.unset index)
  }

lookup :: (Show element, Eq element) => Int -> Transaction element
lookup index =
  Transaction {
    name = "lookup " <> fromString (show index),
    applyToMaybeList = fmap (join . fmap fst . uncons . drop index) get,
    applyToSparseSmallArray = fmap (SparseSmallArray.lookup index) get
  }

elementsUnfoldl :: (Show element, Eq element) => Transaction element
elementsUnfoldl =
  Transaction {
    name = "elementsUnfoldl",
    applyToMaybeList = do
      list <- get
      return $ do
        maybeElement <- Unfoldl.foldable list
        element <- Unfoldl.foldable maybeElement
        return element,
    applyToSparseSmallArray = fmap SparseSmallArray.elementsUnfoldl get
  }

focusAt :: (Show element, Eq result, Show result) => Text -> Focus element Identity result -> Int -> Transaction element
focusAt focusName focus index =
  Transaction {
    name = "focusAt (" <> (fromString . show) index <> " " <> focusName <> ")"
    ,
    applyToMaybeList = do
      list <- get
      case focus of
        Focus conceal reveal -> case splitAt index list of
          (precedingList, elementMaybe : trailingList) -> do
            (result, change) <- lift (maybe conceal reveal elementMaybe)
            case change of
              Focus.Leave -> return ()
              Focus.Set newElement -> put (precedingList <> [Just newElement] <> trailingList)
              Focus.Remove -> put (precedingList <> [Nothing] <> trailingList)
            return result
          _ -> error "Index out of bounds"
    ,
    applyToSparseSmallArray = StateT (SparseSmallArray.focusAt focus index)
  }

focusInsert :: Show element => Int -> element -> Transaction element
focusInsert index element =
  focusAt ("Insert (" <> (fromString . show) element <> ")") (Focus.insert element) index

focusDelete :: Show element => Int -> Transaction element
focusDelete index =
  focusAt ("Delete") Focus.delete index
