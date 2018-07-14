module PrimitiveExtras.IO
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as A


-- * UnliftedArray
-------------------------

generateUnliftedArray :: PrimUnlifted a => Int -> (Int -> IO a) -> IO (UnliftedArray a)
generateUnliftedArray = A.generate

replicateUnliftedArray :: PrimUnlifted a => Int -> IO a -> IO (UnliftedArray a)
replicateUnliftedArray = A.replicateIO

-- * Array
-------------------------

generateArray :: Int -> (Int -> IO a) -> IO (Array a)
generateArray size elementIO =
  do
    array <- newArray size undefined
    let
      loop index =
        if index < size
          then do
            element <- elementIO index
            writeArray array index element
            loop (succ index)
          else unsafeFreezeArray array
      in loop 0

replicateArray :: Int -> IO a -> IO (Array a)
replicateArray size elementIO =
  do
    array <- newArray size undefined
    let
      loop index =
        if index < size
          then do
            element <- elementIO
            writeArray array index element
            loop (succ index)
          else unsafeFreezeArray array
      in loop 0

-- * PrimArray
-------------------------

generatePrimArray :: Prim a => Int -> (Int -> IO a) -> IO (PrimArray a)
generatePrimArray size elementIO =
  do
    array <- newPrimArray size
    let
      loop index =
        if index < size
          then do
            element <- elementIO index
            writePrimArray array index element
            loop (succ index)
          else unsafeFreezePrimArray array
      in loop 0

replicatePrimArray :: Prim a => Int -> IO a -> IO (PrimArray a)
replicatePrimArray size elementIO =
  do
    array <- newPrimArray size
    let
      loop index =
        if index < size
          then do
            element <- elementIO
            writePrimArray array index element
            loop (succ index)
          else unsafeFreezePrimArray array
      in loop 0

traversePrimArrayWithIndexInRange :: Prim a => PrimArray a -> Int -> Int -> (Int -> a -> IO ()) -> IO ()
traversePrimArrayWithIndexInRange primArray from to action =
  let iterate index = if index < to
        then do
          action index $! indexPrimArray primArray index
          iterate (succ index)
        else return ()
      in iterate from

-- * TVarArray
-------------------------

newTVarArray :: a -> Int -> IO (TVarArray a)
newTVarArray a size = TVarArray <$> replicateArray size (newTVarIO a)

freezeTVarArrayAsPrimArray :: Prim a => TVarArray a -> IO (PrimArray a)
freezeTVarArrayAsPrimArray (TVarArray varArray) =
  do
    let size = sizeofArray varArray
    mpa <- newPrimArray size
    forMFromZero_ size $ \ index -> do
      var <- indexArrayM varArray index
      value <- atomically (readTVar var)
      writePrimArray mpa index value
    unsafeFreezePrimArray mpa

modifyTVarArrayAt :: TVarArray a -> Int -> (a -> a) -> IO ()
modifyTVarArrayAt (TVarArray array) index fn =
  do
    var <- indexArrayM array index
    atomically $ modifyTVar' var fn
