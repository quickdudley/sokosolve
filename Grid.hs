module Grid (
  Grid(..),
  Direction(..),
  Tile(..),
  directions,
  step,
  applyStep,
  lookupGrid,
  isBox,
  isClear,
  solved,
  readGrid,
  showGrid
 ) where

import Data.Array
import qualified Data.Set as S
import Data.List

data Grid = Grid {
  gridPlayer :: (Int,Int),
  gridBoxes :: S.Set (Int,Int),
  gridWalls :: Array (Int,Int) Bool,
  gridTargets :: S.Set (Int,Int)
 } deriving (Ord,Eq)

data Direction = North | East | South | West deriving (Ord,Eq)
data Tile = Wall | Clear | Box | Target | Player | PT | BT deriving (Eq)

directions = [North,East,South,West]

step North (x,y) = (x,y-1)
step East (x,y) = (x+1,y)
step South (x,y) = (x,y+1)
step West (x,y) = (x-1,y)

instance Show Direction where
  show North = "↑"
  show East = "→"
  show South = "↓"
  show West = "←"

lookupGrid l g
 | not $ inRange (bounds $ gridWalls g) l = Wall
 | gridWalls g ! l = Wall
 | S.member l (gridTargets g) = case () of
    () | gridPlayer g == l -> PT
    () | S.member l (gridBoxes g) -> BT
    () | otherwise -> Target
 | S.member l (gridBoxes g) = Box
 | gridPlayer g == l = Player
 | otherwise = Clear

isWall l g = let
  w = gridWalls g
  in not (inRange (bounds w) l) || w ! l

isBox l g = S.member l $ gridBoxes g

isClear l g = not (isWall l g || isBox l g)

applyStep d g = let
  p = gridPlayer g
  p' = step d p
  b' = step d p'
  pushBox = Just $ g {
    gridPlayer = p',
    gridBoxes = S.insert b' $ S.delete p' $ gridBoxes g
   }
  in case (lookupGrid p' g, lookupGrid b' g) of
    (Clear,_) -> Just $ g {gridPlayer = p'}
    (Box,Clear) -> pushBox
    (Box,Target) -> pushBox
    (BT,Clear) -> pushBox
    (BT,Target) -> pushBox
    _ -> Nothing

{-
 - Input format:
 - * : Wall
 - . or space : Clear
 - p : Player
 - # : Box
 - _ : Target
 - + : Box on target
 - P : Player on target
-}
readGrid t = let
  l = lines t
  h = length l - 1
  w = maximum (map length l) - 1
  i = do
    (y,r) <- zip [0..] l
    (x,c) <- zip [0..] r
    return ((x,y),c)
  in Grid {
    gridPlayer = case filter ((`elem` "pP") . snd) i of
      [] -> (-1,-1)
      ((p,_):_) -> p,
    gridWalls = array ((0,0),(w,h)) [(l,c == '*') | (l,c) <- i],
    gridBoxes = S.fromList $ map fst $ filter ((`elem` "#+") . snd) i,
    gridTargets = S.fromList $ map fst $ filter ((`elem` "_+P") . snd) i
   }

showGrid g = unlines $ map (map gc) pts where
  pts = let
    ((x',y'),(w,h)) = bounds (gridWalls g)
    in map (\y -> map (\x -> (x,y)) [x' .. w]) [y' .. h]
  gc l = case lookupGrid l g of
    Wall -> '*'
    Clear -> '.'
    Player -> 'p'
    Box -> '#'
    Target -> '_'
    BT -> '+'
    PT -> 'P'

solved g = gridBoxes g == gridTargets g

