{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Cliques (cliques, rootcliques) where


{-
  Given vertices v1 and v2 from graph g, determine if v2 is numerically
  less than v1 and the two vertices are adjacent.

  The directional requirement allows the cliques function to eliminate
  permutations.
-}
areAdj g v1 v2 = (v2 < v1) && (any (v1 ==) (g !! v2))


{-
  Given vertex v and a set of vertices vs from graph g, determine if
  the areAdj predicate is satisfied for all pairs (v, x) with x from vs.
-}
allAdj g vs v = all (areAdj g v) vs


{-
  Given a graph g and a set of cliques c, find all cliques that can be
  constructed by adding a single vertex in g to a clique in c.
-}
nextClique g c = [y ++ [w] | y <- c,
                             w <- filter (allAdj g y) (take (length g) (iterate (1 +) 0))]


{-
  Given a graph g, find all cliques of size n.
-}
cliques g 0  = [[]]
cliques g n  = (iterate (nextClique g) [[]]) !! n


{-
  Given a graph g, find all cliques of size n that
  include vertex 0.
-}
rootcliques g 0 = [[]]
rootcliques g n = (iterate (nextClique g) [[0]]) !! (n - 1)

