module Hungarian (
  hungarian
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
    nJobs = length jobs
    dim = max (length workers) nJobs
    jobsArray = listArray (0, nJobs - 1) jobs

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
            Just _ -> c (n + 1)
            Nothing -> return $ Just n
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
              fmap (minimumBy (compare `on` \(a,_,_) -> a) . catMaybes) $
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
            lift $ writeArray parentWorkerByCommittedJob minSlackJob (Just minSlackWorker)
            mw <- lift $ readArray matchWorkerByJob minSlackJob
            case mw of
              Nothing -> do -- Apply the augmenting path
                committedJob <- lift $ newSTRef (Just minSlackJob)
                parentWorker <- lift $
                  readArray parentWorkerByCommittedJob minSlackJob >>= newSTRef
                callCC $ \endAugmentingPath -> forever $ do
                  temp <- lift $
                    readSTRef parentWorker >>= \pw -> case pw of
                      Just pw' -> readArray matchJobByWorker pw'
                      Nothing -> return Nothing
                  pw <- lift $ readSTRef parentWorker
                  cj <- lift $ readSTRef committedJob
                  lift $ case (pw,cj) of
                    (Just pw',Just cj') -> match pw' cj'
                    (Nothing,Just cj') -> writeArray matchWorkerByJob cj' Nothing
                    (Just pw',Nothing) -> writeArray matchJobByWorker pw' Nothing
                    _ -> return ()
                  lift $ writeSTRef committedJob temp
                  case temp of
                    Nothing -> endAugmentingPath ()
                    Just tmp -> lift $
                      readArray parentWorkerByCommittedJob tmp >>=
                      writeSTRef parentWorker
                endPhase ()
              Just worker -> lift $ do -- Update slack values
                writeArray committedWorkers worker True
                forM_ [0 .. dim - 1] $ \j -> do
                  pw <- readArray parentWorkerByCommittedJob j
                  case pw of
                    Just _ -> return ()
                    Nothing -> do
                      c <- readArray costMatrix (worker,j)
                      lw <- readArray labelByWorker worker
                      lj <- readArray labelByJob j
                      let slack = c - lw - lj
                      msv <- readArray minSlackValueByJob j
                      when (msv > slack) $ do
                        writeArray minSlackValueByJob j slack
                        writeArray minSlackWorkerByJob j worker
          ml
  ml
  
  -- Format the result
  fmap catMaybes $ forM (zip workers [0..]) $ \(w,wi) -> do
    ji' <- readArray matchJobByWorker wi
    case ji' of
      Nothing -> return Nothing
      Just ji -> do
        if ji >= nJobs
          then return Nothing
          else return $ Just (w,jobsArray ! ji, originalCosts ! (wi,ji))

