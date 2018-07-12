module PrimitiveExtras.IO
where

import PrimitiveExtras.Prelude
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as A


generateUnliftedArray :: PrimUnlifted a => Int -> (Int -> IO a) -> IO (UnliftedArray a)
generateUnliftedArray = A.generate

replicateUnliftedArray :: PrimUnlifted a => Int -> IO a -> IO (UnliftedArray a)
replicateUnliftedArray = A.replicateIO

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

traversePrimArrayWithIndexConcurrently ::
  Prim a =>
  PrimArray a ->
  Int ->
  (Int -> a -> IO ()) ->
  IO (IO ()) {-^ An action, which blocks until the concurrent computation is finished. -}
traversePrimArrayWithIndexConcurrently primArray threadsAmount action =
  let size = sizeofPrimArray primArray
      maxIndex = pred size
      step = div size threadsAmount
      in case step of
        0 -> itraversePrimArray_ action primArray $> return ()
        _ -> do
          semaphore <- newTVarIO threadsAmount
          let
            startThreads index = if index >= size
              then return ()
              else let
                !nextIndex = step + index
                in do
                  forkIO $ do
                    traversePrimArrayWithIndexInRange primArray index (min nextIndex size) action
                    atomically (modifyTVar' semaphore pred)
                  startThreads nextIndex
            in do
              startThreads 0
              return $ atomically $ do
                activeThreads <- readTVar semaphore
                guard (activeThreads == 0)

