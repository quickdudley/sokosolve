# sokosolve
Sokoban solver in Haskell

Solves Sokoban using 2 nested invocations of the A* algorithm, giving move-optimal solutions. It is currently a little faster than naive breadth-first search, and uses less memory.

Tunnel detection is used to reduce the number of states examined (if a box is pushed more than one space into a 1 unit wide tunnel, it will automatically be pushed to the end of the tunnel)

2 methods of deadlock detection are used. One builds a whitelist of places from where it is ever possible to push a box to any target. The other considers a box deadlocked if there are walls or other deadlocks adjacent to it at right angles.

The cost heuristic used in the outer invocation of A* is given by the minimum matching from boxes to targets, with manhattan distance as the edge cost.

To compile:
===========

If using GHC: run `ghc sokosolve.hs`.

sokosolve is not currently cabalized because it has no dependencies outside of the Haskell platform.
