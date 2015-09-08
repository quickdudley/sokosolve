module Hungarian (

 ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Cont
import Control.Monad.Trans.Class
import Data.Array.ST
import Data.STRef

hungarian :: (a -> b -> Integer) -> [a] -> [b] -> [(a,b,Integer)]
hungarian mf workers jobs = runST $ do
  let dim = max (length workers) (length jobs)
  costMatrix <- newArray ((0,0),(dim - 1, dim - 1)) 0 :: ST s (STArray s (Int,Int) Integer)
  forM_ (zip workers [0..]) $ \(we,w) -> forM (zip jobs [0..]) $ \(je,j) ->
    writeArray costMatrix (w,j) (mf we je)
  originalCosts <- freeze costMatrix :: ST s (Array (Int,Int) Integer)
  -- subtract minimum from each row and column
  forM_ [map (c o) [0 .. dim - 1] | c <- [flip (,), (,)], o <- [0 .. dim - 1]] $ \l -> do
    m <- fmap minimum $ forM l $ readArray costMatrix
    forM_ l $ \x -> do
      v <- readArray costMatrix x
      writeArray costMatrix x (v - m)
  undefined
