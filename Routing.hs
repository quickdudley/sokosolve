module Routing (
  astar,
  dijkstra
 ) where

import qualified Data.Heap as H
import qualified Data.Map as M
import Data.Monoid ((<>))
import Control.Monad.State

astar :: (Ord n) =>
  (n -> Integer) ->
  (n -> Bool) ->
  (n -> [(Integer,s,n)]) ->
  n ->
  [([s],n,Integer)]
astar h g b s = flip evalState (H.singleton 0 (s,id,0)) (go M.empty) where
  go v = do
    n <- pop
    case n of
      Nothing -> return []
      Just (p, (a,s,c)) -> if M.member a v
        then go v
        else let
          nxt = map (\(c',s',n') -> let
            c'' = c + c'
            in (c'' + h n',(n', s . (s':), c''))
           ) $ b a
          na = pushAll nxt >> go v'
          v' = M.insert a (s []) v
          in if g a then fmap ((s [], a, c):) na else na

dijkstra ::
  (Ord n) => (n -> Bool) -> (n -> [(Integer,s,n)]) -> n -> [([s],n,Integer)]
dijkstra = astar (const 0)

pop :: (Ord p) => State (H.Heap p a) (Maybe (p,a))
pop = state $ \h -> case H.pop h of
  Nothing -> (Nothing, h)
  Just (p,a,h') -> (Just (p,a), h')

pushAll :: (Ord p) => [(p,a)] -> State (H.Heap p a) ()
pushAll = state . ((,) () .) . (<>) . H.fromList

