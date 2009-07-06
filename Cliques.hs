{-
  Find cliques of a particular size on a graph.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Cliques (cliques, rootcliques) where

import Data.Set


{-
  Type definitions.
-}
type Vert = Int
type Clique = Set Vert
type Graph = [Set Vert]


{-
  Given a clique c and a vertex v on graph g, determine if v extends c.

  A vertex extends a clique if the vertex is adjacent to all vertices
  in the clique.
-}
extends :: Graph -> Clique -> Vert -> Bool
extends g c v = all (\x -> member v (g !! x)) (elems c)


{-
  Given a graph g and a clique c, find the list of all vertices in g that
  extend c and are numerically greater than c.
-}
extendingVerts :: Graph -> Clique -> [Vert]
extendingVerts g c = Prelude.filter (extends g c) (take n [b..])
                     where b = if  Data.Set.null c
                               then 0
                               else 1 + (findMax c)
                           n = (length g) - b


{-
  Given a graph g and a list of cliques s, find all cliques in g that
  contain a clique from s and one additional vertex.
-}
biggerCliques :: Graph -> [Clique] -> [Clique]
biggerCliques g s = [insert w y | y <- s, w <- extendingVerts g y]


{-
  Given a graph g, find all cliques of size n.
-}
cliques :: Graph -> Int -> [Clique]
cliques g 0  = [empty]
cliques g n  = (iterate (biggerCliques g) [empty]) !! n


{-
  Given a graph g, find all cliques of size n that
  include vertex 0.
-}
rootcliques :: Graph -> Int -> [Clique]
rootcliques g 0 = [empty]
rootcliques g n = (iterate (biggerCliques g) [singleton 0]) !! (n - 1)

