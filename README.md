# sokosolve
Sokoban solver in Haskell

Solves Sokoban using 2 nested invocations of the A* algorithm. It is currently about the same speed as breadth-first search, although uses less memory.

The performance on typical levels is expected to improve once the following enhancements have been made:

1. Add a heuristic to the outer invocation of A* (both invocations currently use a constant 0, behaving identically to Dijkstra's algorithm). The planned heuristic is the total cost as given by the Hungarian algorithm matching crates to target squares, with manhattan distance as the edge cost.
2. Add simple deadlock detection.
3. Tunnel detection.
