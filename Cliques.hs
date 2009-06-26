{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Cliques (cliques) where

{-
  Given vertices v1 and v2 from graph g, determine if v2 is numerically
  greater than v1 and the two vertices and adjacent.

  The directional requirement allows the cliques function to eliminate
  permutations.
-}
areAdj g v1 v2 = (v2 > v1) && (any (v1 ==) (g !! v2))


{-
  Given vertex v and a set of vertices vs from graph g, determine if
  the areAdj predicate is satisfied for all pairs (v, x) with x from vs.
-}
allAdj g v vs = all (areAdj g v) vs


{-
  Find all cliques of size n on graph g.

  This function works by creating all lists [y : w] where y is a vertex
  in g and w is a smaller clique than the one desired, and y is adjacent
  to and numerically less than all vertices in w.
-}
cliques g 0 = [[]]
cliques g n = [y : w | y <- take (length g) (iterate (1 +) 0),
                       w <- filter (allAdj g y) (cliques g (n - 1))]


