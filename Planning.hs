module Planning (
  solve
 ) where

import Routing
import Grid

import Data.List

solve g = undefined

clearSteps g = let
  p = gridPlayer g
  in map (\(d,p',_) -> (1,d,g {gridPlayer = p'})) $
    filter (\(_,_,t) -> t `elem` [Clear,Target,Player,PT]) $
    map (\d -> let p' = step d p in (d,p',lookupGrid p' g)) directions

byBox g = let
  p = gridPlayer g
  in any ((`elem` [Box,BT]) . flip lookupGrid g . flip step p) directions

