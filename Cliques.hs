{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

--module Cliques (cliques) where
module Cliques (cliques, rootcliques) where

{-
  Given vertices v1 and v2 from graph g, determine if v2 is numerically
  less than v1 and the two vertices and adjacent.

  The directional requirement allows the cliques function to eliminate
  permutations.
-}
areAdj g v1 v2 = (v2 < v1) && (any (v1 ==) (g !! v2))

--areAdj' g v1 v2 = (v2 < v1) && (any (v1 ==) (g !! v2))
--lutAdj g = [areAdj' g x y | x <- z, y <- z]
--           where z = take (length g) (iterate (1 +) 0)
--areAdj g v1 v2 = (lutAdj g) !! (v1 * (length g) + v2)

{-
  Given vertex v and a set of vertices vs from graph g, determine if
  the areAdj predicate is satisfied for all pairs (v, x) with x from vs.
-}
allAdj g vs v = all (areAdj g v) vs


{-
  Find all cliques of size n on graph g (m vertices)

  This function works by creating all lists [y : w] where y is a vertex
  in g and w is a smaller clique than the one desired, and y is adjacent
  to and numerically less than all vertices in w.
-}
--cliquesO g m 0 = [[]]
--cliquesO g m n = [y : w | y <- take m (iterate (1 +) 0),
--                         w <- filter (allAdj g y) (cliquesO g m (n - 1))]


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

