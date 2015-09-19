module Deadlocks (
  whiteList,
  multiDeadlock
 ) where

import Grid
import Routing
import Data.Array
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State

pullSteps :: Grid -> [(Integer,Direction,Grid)]
pullSteps g = let
  p = gridPlayer g
  in map (\(b,p',d) -> (1,d,g {
      gridPlayer = p',
      gridBoxes = S.insert p $ S.delete b $ gridBoxes g
     })) $
    filter (\(b,p',_) -> isBox b g && isClear p' g) $
    map (\d -> (step d p, stride (-1) d p,d)) directions

pullStart :: Grid -> [Grid]
pullStart g = do
  t <- S.toList $ gridTargets g
  d <- directions
  let p = step d t
  guard $ not $ isWall p g
  return $ g {
    gridPlayer = p,
    gridBoxes = S.singleton t,
    gridTargets = S.empty
   }

changePull :: Grid -> [(Integer,[Direction],Grid)]
changePull g = let
  esq = do
    b <- S.toList $ gridBoxes g
    d <- directions
    let esq = step d b
    guard $ isClear esq g
    return esq
  r = zipWith (flip const) esq $ dijkstra
    (`elem` esq)
    (\l -> map (\(c,d,g') -> (c,d,gridPlayer g')) $ clearSteps $ g {gridPlayer = l})
    (gridPlayer g)
  in map (\(d,l,c) -> (c,d,g {gridPlayer = l})) r

whiteList :: Grid -> Array (Int,Int) Bool
whiteList g = let
  s = pullStart g
  h = gridPlayer g
  subpath g0 = do
    (c1,s1,g1) <- changePull g0
    (c2,s2,g2) <- pullSteps g1
--    guard $ backHome h g2
    return (c1 + c2, s1 ++ [s2], g2)
  bp = concatMap (\(_,b,_) -> S.toList $ gridBoxes b) $ dijkstra'
    (const True)
    subpath
    (pullStart g)
  ab = bounds $ gridWalls g
  in array ab $ map (flip (,) False) (range ab) ++ map (flip (,) True) bp

backHome h g = case astar
  (\p -> max 0 $ fromIntegral $ manhattan h p - 1)
  (== h)
  (\p -> do
    d <- directions
    let p' = step d p
    guard $ isClear p' g
    return (1,d,p')
   )
  (gridPlayer g)
 of
  [] -> False
  _ -> True

multiDeadlock :: Direction -> Grid -> Bool
multiDeadlock md g = let
  ub = gridBoxes g S.\\ gridTargets g
  p = gridPlayer g
  b1 = step md p
  check b 
    | isWall b g = return True
    | not (isBox b g) = return False
    | otherwise = do
      l <- fmap (S.member b) get
      if l
        then return True
        else do
          modify (S.insert b)
          s <- forM directions $ \d -> check (step d b)
          case s of
            [True,True,_,_] -> return True
            [_,True,True,_] -> return True
            [_,_,True,True] -> return True
            [True,_,_,True] -> return True
            _ -> modify (S.delete b) >> return False
  in if isTarget b1 g
    then any (\t -> let
      b = step (t md) b1
      in if S.member b ub
        then evalState (check b) S.empty
        else False
     ) [turnLeft,id,turnRight]
    else evalState (check b1) S.empty


