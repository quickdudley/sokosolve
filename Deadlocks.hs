module Deadlocks (
  whiteList
 ) where

import Grid
import Routing
import Data.Array
import qualified Data.Set as S
import Control.Monad

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
  in undefined

backHome h g = case astar
  (\p -> max 0 $ fromIntegral $ manhattan h p - 1)
  (== h)
  (\p -> do
    d <- directions
    let p' = step d p
    guard $ isClear p' g
    return (1,d,p)
   )
  (gridPlayer g)
 of
  [] -> False
  _ -> True

