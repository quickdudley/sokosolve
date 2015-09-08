module Hungarian (

 ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Cont
import Control.Monad.Trans.Class
import Data.Array.ST
import Data.Array
import Data.STRef

hungarian :: (a -> b -> Integer) -> [a] -> [b] -> [(a,b,Integer)]
hungarian mf workers jobs = runST $ do
  let dim = max (length workers) (length jobs)

  -- Set up cost matrix
  costMatrix <- newArray ((0,0),(dim - 1, dim - 1)) 0 :: ST s (STArray s (Int,Int) Integer)
  forM_ (zip workers [0..]) $ \(we,w) -> forM (zip jobs [0..]) $ \(je,j) ->
    writeArray costMatrix (w,j) (mf we je)
  originalCosts <- fmap (id :: Array (Int,Int) Integer -> Array (Int,Int) Integer) $ freeze costMatrix

  -- Subtract minimum from each row and column
  forM_ [map (c o) [0 .. dim - 1] | c <- [flip (,), (,)], o <- [0 .. dim - 1]] $ \l -> do
    m <- fmap minimum $ forM l $ readArray costMatrix
    forM_ l $ \x -> do
      v <- readArray costMatrix x
      writeArray costMatrix x (v - m)

  -- Compute initial feasible labeling (just following example, I suspect
  -- the initial labels are all 0 because of the above step)
  labelByWorker <- newArray (0, dim - 1) 0 :: ST s (STArray s Int Integer)
  labelByJob <- newArray (0, dim - 1) 0 :: ST s (STArray s Int Integer)
  forM_ [0 .. dim - 1] $ \j -> do
    m <- fmap minimum $ forM [0 .. dim - 1] $ \w -> readArray costMatrix (w,j)
    writeArray labelByJob j m

  matchJobByWorker <- newArray (0, dim - 1) Nothing :: ST s (STArray s Int (Maybe Int))
  matchWorkerByJob <- newArray (0, dim - 1) Nothing :: ST s (STArray s Int (Maybe Int))
  let
    -- I guess this is a strange place to define a function, but it had to
    -- be below the above 2 "newArray" lines and before its first use.
    match w j = do
      writeArray matchJobByWorker w (Just j)
      writeArray matchWorkerByJob j (Just w)

  -- Initial greedy matching
  forM_ [0 .. dim - 1] $ \w -> forM [0 .. dim - 1] $ \j -> do
    c <- readArray costMatrix (w,j)
    lw <- readArray labelByWorker w
    lj <- readArray labelByJob j
    if c - lw - lj == 0
      then do
        wm <- readArray matchJobByWorker w
        jm <- readArray matchWorkerByJob j
        case (wm,jm) of
          (Nothing,Nothing) -> match w j
          _ -> return ()
      else return ()
  undefined

