module Hungarian (

 ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Cont
import Control.Monad.Trans.Class
import Data.Array.ST
import Data.Array
import Data.STRef
import Data.List
import Data.Maybe
import Data.Function (on)

hungarian :: (a -> b -> Integer) -> [a] -> [b] -> [(a,b,Integer)]
hungarian mf workers jobs = runST $ do
  let
    dim = max (length workers) (length jobs)
    jobsArray = listArray (0, dim - 1) jobs

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
    fetchUnmatchedWorker = let
      c n = if n >= dim
        then return Nothing
        else do
          j <- readArray matchJobByWorker n
          case j of
            Nothing -> c (n + 1)
            _ -> return j
      in c 0

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

  -- The main loop of the algorithm
  let
    ml = do
      uw' <- fetchUnmatchedWorker
      case uw' of
        Nothing -> return ()
        Just uw -> do
          -- Initialize run
          committedWorkers <- newArray (0,dim-1) False :: ST s (STArray s Int Bool)
          parentWorkerByCommittedJob <- newArray (0,dim-1) Nothing :: ST s (STArray s Int (Maybe Int))
          minSlackValueByJob <- newArray (0,dim-1) 0 :: ST s (STArray s Int Integer)
          minSlackWorkerByJob <- newArray (0,dim-1) uw :: ST s (STArray s Int Int)
          writeArray committedWorkers uw True
          luw <- readArray labelByJob uw
          forM_ [0 .. dim - 1] $ \j -> do
            c <- readArray costMatrix (uw,j)
            lj <- readArray labelByJob j
            writeArray minSlackValueByJob j (c - luw - lj)

          -- Main calculation
          flip runContT return $ callCC $ \endPhase -> forever $ do
            (minSlackValue,minSlackWorker,minSlackJob) <- lift $
              fmap (maximumBy (compare `on` \(a,_,_) -> a) . catMaybes) $
              forM [0 .. dim - 1] $ \j -> do
                pw <- readArray parentWorkerByCommittedJob j
                case pw of
                  Nothing -> do
                    sv <- readArray minSlackValueByJob j
                    sw <- readArray minSlackWorkerByJob j
                    return $ Just (sv,sw,j)
                  _ -> return Nothing
            when (minSlackValue > 0) $ lift $ do
              -- Update Labeling
              forM_ [0 .. dim - 1] $ \w -> do
                cw <- readArray committedWorkers w
                when cw $ do
                  l <- readArray labelByWorker w
                  writeArray labelByWorker w (l + minSlackValue)
              forM_ [0 .. dim - 1] $ \j -> do
                pw <- readArray parentWorkerByCommittedJob j
                case pw of
                  Just _ -> do
                    lj <- readArray labelByJob j
                    writeArray labelByJob j (lj - minSlackValue)
                  Nothing -> do
                    ms <- readArray minSlackValueByJob j
                    writeArray minSlackValueByJob j (ms - minSlackValue)
            
            undefined
          ml
  ml
  undefined

