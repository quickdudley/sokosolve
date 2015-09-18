module Planning (
  solve
 ) where

import Routing
import Grid
import Deadlocks
import Hungarian

import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Array
import Control.Monad

heuristic g0 = let
  dm = pushDistances g0
  in \g -> let
    m = hungarian
     (\b t -> maybe (fromIntegral (maxBound :: Int)) id (dm ! t ! b))
     (S.toList $ gridBoxes g)
     (S.toList $ gridTargets g)
    in sum $ map (\(_,_,c) -> c) m

solve g = case astar (heuristic g) solved (walks $ whiteList g) g of
  [] -> Nothing
  ((s,_,_):_) -> Just $ concat s

byBox g = let
  p = gridPlayer g
  in any (flip isBox g . flip step p) directions

inTunnel g d = let
  p = gridPlayer g
  flanks = map (\t -> step (t d) p) [turnLeft,turnRight]
  foreflanks = map (step d) flanks
  w = flip isWall g
  in not (isTarget (step d p) g) && all w flanks && any w foreflanks

pushTunnel :: (Integer, Direction, Grid) ->
  Maybe (Integer, [Direction], Grid)
pushTunnel (c,d,g) = if inTunnel g d
  then do
    g1 <- applyStep d g
    (c',s,g') <- pushTunnel (c + 1, d, g1)
    return (c', d:s, g')
  else Just (c,[d],g)

pushSteps :: Array (Int,Int) Bool -> Grid -> [(Integer, [Direction], Grid)]
pushSteps w g = let
  p = gridPlayer g
  in mapMaybe (\(d,p',b') -> pushTunnel (1,d,g {
      gridPlayer = p',
      gridBoxes = S.delete p' $ S.insert b' $ gridBoxes g
     }) >>= \t@(_,_,g) -> if multiDeadlock d g then Nothing else Just t) $
    filter (\(_,p',b') -> isBox p' g && w ! b' && isClear b' g
     ) $
    map (\d -> let p' = step d p in (d,p',step d p')) directions

clearWalks :: Grid -> [(Integer,[Direction],Grid)]
clearWalks g = let
  -- Originally this only returned tiles adjacent to boxes
  -- but that's no longer necessary because the relationship
  -- between this function and pushSteps was changed, making
  -- the extra check redundant.
  r = dijkstra (const True) clearSteps g
  in map (\(s,g',c) -> (c,s,g')) r

wrapStep :: (n -> [(Integer,s,n)]) -> n -> [(Integer,[s],n)]
wrapStep b = map (\(c,s,n) -> (c,[s],n)) . b

walks :: Array (Int,Int) Bool -> Grid -> [(Integer,[Direction],Grid)]
walks w g = do
  (c1,p1,g1) <- clearWalks g
  (c2,p2,g2) <- pushSteps w g1
  return (c1 + c2, p1 ++ p2, g2)

