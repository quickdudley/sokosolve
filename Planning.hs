module Planning (
  solve
 ) where

import Routing
import Grid

import Data.List
import Data.Maybe

--TODO: The Hungarian algorithm is a better heuristic
heuristic = const 0

solve g = case astar heuristic solved walks g of
  [] -> Nothing
  ((s,_,_):_) -> Just $ concat s

clearSteps :: Grid -> [(Integer, Direction, Grid)]
clearSteps g = let
  p = gridPlayer g
  in map (\(d,p',_) -> (1,d,g {gridPlayer = p'})) $
    filter (\(_,_,t) -> t `elem` [Clear,Target,Player,PT]) $
    map (\d -> let p' = step d p in (d,p',lookupGrid p' g)) directions

byBox g = let
  p = gridPlayer g
  in any ((`elem` [Box,BT]) . flip lookupGrid g . flip step p) directions

pushSteps :: Grid -> [(Integer, Direction, Grid)]
pushSteps g = let
  p = gridPlayer g
  in mapMaybe (\(d,_,_) -> fmap (\g' -> (1,d,g')) $ applyStep d g) $
    filter (\(_,p',b') -> let
      [pt,bt] = map (flip lookupGrid g) [p',b']
      in pt `elem` [Box,BT] && bt `elem` [Clear,Target]
     ) $
    map (\d -> let p' = step d p in (d,p',step d p')) directions

clearWalks :: Grid -> [(Integer,[Direction],Grid)]
clearWalks g = let
  r = dijkstra byBox clearSteps g
  in map (\(s,g',c) -> (c,s,g')) r

wrapStep :: (n -> [(Integer,s,n)]) -> n -> [(Integer,[s],n)]
wrapStep b = map (\(c,s,n) -> (c,[s],n)) . b

walks :: Grid -> [(Integer,[Direction],Grid)]
walks g = do
  (c1,p1,g1) <- clearWalks g
  (c2,p2,g2) <- pushSteps g1
  return (c1 + c2, p1 ++ [p2], g2)

-- wrapStep pushSteps g ++ clearWalks g

