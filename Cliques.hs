{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Cliques (cliques, rootcliques, cliquesS, rootcliquesS) where

import Data.List
import Data.Set

{-
  Given a set of vertices vs and vertex v, determine if all the vertices
  in vs are adjacent to v.
-}
allAdj g vs v = all (\x -> any (v ==) (g !! x)) vs
--allAdj g vs v = all (\x -> any (x ==) (g !! v)) vs
allAdjS g vs v = all (\x -> Data.Set.member v (g !! x)) (Data.Set.elems vs)


{-
  Given a graph g and a clique x, find all vertices in g that extend x.

  Vertices that extend x are vertices that are numerically greater than all
  the vertices in x (any vertex less than that would already have been
  considered by earlier calls) and is adjacent to all vertices in x.
-}
extendClique g x = Data.List.filter (allAdj g x) (take n (iterate (1 +) b))
                   where b = if   Data.List.null x
                             then 0
                             else 1 + (maximum x)
                         n = (length g) - b

extendCliqueS :: [Set Int] -> Set Int -> Set Int
extendCliqueS g x = Data.Set.filter (allAdjS g x) (Data.Set.fromList (take n (iterate (1 +) b)))
                    where b = if   Data.Set.null x
                              then 0
                              else 1 + (maximum (Data.Set.elems x))
                          n = (length g) - b


{-
  Given a graph g and a set of cliques c, find all cliques that can be
  constructed by adding a single vertex in g to a clique in c.
-}
nextClique g c = [y ++ [w] | y <- c, w <- extendClique g y]
--nextClique g c = [w : y | y <- c, w <- extendClique g y]

nextCliqueS :: [Set Int] -> [Set Int] -> [Set Int]
nextCliqueS g c = [Data.Set.insert w y | y <- c, w <- Data.Set.elems (extendCliqueS g y)]


{-
  Given a graph g, find all cliques of size n.
-}
cliques g 0  = [[]]
cliques g n  = (iterate (nextClique g) [[]]) !! n

cliquesS g 0  = [Data.Set.empty]
cliquesS g n  = (iterate (nextCliqueS g) [Data.Set.empty]) !! n


{-
  Given a graph g, find all cliques of size n that
  include vertex 0.
-}
rootcliques g 0 = [[]]
rootcliques g n = (iterate (nextClique g) [[0]]) !! (n - 1)

rootcliquesS g 0 = [Data.Set.empty]
rootcliquesS g n = (iterate (nextCliqueS g) [Data.Set.singleton 0]) !! (n - 1)

