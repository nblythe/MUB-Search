{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Cliques (cliques, rootcliques) where

import Data.List

{-
  Given a set of vertices vs and vertex v, determine if all the vertices
  in vs are adjacent to v.
-}
allAdj g vs v = all (\x -> any (v ==) (g !! x)) vs
--allAdj g vs v = all (\x -> any (x ==) (g !! v)) vs


{-
  Given a graph g and a clique x, find all vertices in g that extend x.

  Vertices that extend x are vertices that are numerically greater than all
  the vertices in x (any vertex less than that would already have been
  considered by earlier calls) and is adjacent to all vertices in x.
-}
extendClique g x = filter (allAdj g x) (take n (iterate (1 +) b))
                  where b = if   null x
                            then 0
                            else 1 + (maximum x)
                        n = (length g) - b


{-
  Given a graph g and a set of cliques c, find all cliques that can be
  constructed by adding a single vertex in g to a clique in c.
-}
nextClique g c = [y ++ [w] | y <- c, w <- extendClique g y]
--nextClique g c = [w : y | y <- c, w <- extendClique g y]


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


