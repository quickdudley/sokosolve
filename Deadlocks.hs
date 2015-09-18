module Deadlocks (
  whiteList,
  multiDeadlock,
  pushDistances
 ) where

import Grid
import Routing
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

pullSteps :: Grid -> [(Integer,Direction,Grid)]
pullSteps g = let
  p = gridPlayer g
  in map (\(b,p',d) -> (1,turnAround d,g {
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

subpath g0 = do
  (c1,s1,g1) <- changePull g0
  (c2,s2,g2) <- pullSteps g1
  return (c1 + c2, s1 ++ [s2], g2)

whiteList :: Grid -> Array (Int,Int) Bool
whiteList g = let
  s = pullStart g
  h = gridPlayer g
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
  in not (isTarget b1 g) && evalState (check b1) S.empty

-- This function doesn't belong in this module thematically speaking, but uses
-- several of the helper functions defined here
pushDistances :: Grid -> Array (Int,Int) (Array (Int,Int) (Maybe Integer))
pushDistances g = let
  b = bounds $ gridWalls g
  go l = array b $ [(x,Nothing) | x <- range b] ++ if isWall l g
    then []
    else let
      s = do
        let s1 = let x = S.singleton l in g {gridTargets = x, gridBoxes = x}
        d <- directions
        let p = step d l
        guard $ not (isWall p s1)
        return $ s1 {gridPlayer = p }
      in map (\(l',c) -> (l', Just c)) $
        M.toList $ M.fromListWith min $
        map (\(_,gp,c) -> (S.findMin $ gridBoxes gp,c)) $
        dijkstra' (const True) subpath s
  in array b [(l, go l) | l <- range b]

