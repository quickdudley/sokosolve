module Deadlocks (

 ) where

import Grid
import Data.Array
import qualified Data.Set as S

pullSteps :: Grid -> [(Integer,Direction,Grid)]
pullSteps g = let
  p = gridPlayer g
  in map (\(b,p',d) -> (1,d,g {
      gridPlayer = p',
      gridBoxes = S.insert p $ S.delete b $ gridBoxes g
     })) $
    filter (\(b,p',_) -> isBox b g && isClear p' g) $
    map (\d -> (step d p, stride (-1) d p,d)) directions

