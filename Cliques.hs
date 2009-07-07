{-
  Find cliques of a particular size on a undirected graph.

  2009 Nathan Blythe, Dr. Oscar Boykin

  A graph should be represented by the sparse form (a list of sets) of
  the lower triangular component of its adjacency matrix.
-}

--module Cliques (cliques, rootcliques) where
module Cliques where

import Data.Set


{-
  Type definitions.
-}
type Vert = Int
type Clique = Set Vert
type Graph = [Set Vert]


{-
  Compute the intersection of all sets in a list.
-}
--intersections (xH : xT) = if   Prelude.null xT
--                          then xH
--                          else intersection xH (intersections xT)

intersections s = if   1 == (Data.Set.size s)
                  then m
                  else intersection m (intersections t)
                  where (m, t) = deleteFindMin s


{-
  Given a graph g and clique c, find the set of all vertices that extend c.

  A vertex that extends a clique is a vertex that is adjacent to all
  vertices in that clique.
-}
extendingVerts :: Graph -> Clique -> Set Vert
--extendingVerts g c = intersections (Prelude.map (g !!) (elems c))
extendingVerts g c = intersections (Data.Set.map (g !!) c)

{-
  Given a graph g and a list of cliques s, find all cliques in g that
  contain a clique from s and one additional vertex.
-}
biggerCliques :: Graph -> [Clique] -> [Clique]
biggerCliques g s = [insert w y | y <- s, w <- elems (extendingVerts g y)]


{-
  Set containing all vertices.
-}
allVerts :: Graph -> Set Vert
allVerts g = fromList [0 .. (length g) - 1]


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

