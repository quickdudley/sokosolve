# sokosolve
Sokoban solver in Haskell

Solves Sokoban using 2 nested invocations of the A* algorithm. It is currently a little faster than naive breadth-first search, and uses less memory.

Tunnel detection is used to reduce the number of states examined (if a box is pushed more than one space into a 1 unit wide tunnel, it will automatically be pushed to the end of the tunnel)

The performance on typical levels is expected to improve once the following enhancements have been made:

1. Add a heuristic to the outer invocation of A* (both invocations currently use a constant 0, behaving identically to Dijkstra's algorithm). The planned heuristic is the total cost as given by the Hungarian algorithm matching crates to target squares, with manhattan distance as the edge cost.
2. Add simple deadlock detection.
