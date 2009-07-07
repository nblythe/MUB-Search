{-
  Find cliques of a particular size on a graph.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

--module Cliques (cliques, rootcliques, cliquesH, rootcliquesH) where
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
intersections (xH : xT) = if   Prelude.null xT
                          then xH
                          else intersection xH (intersections xT)


{-
  Given a graph g and clique c, find all vertices that extend c.

  A vertex that extends a clique is a vertex that is adjacent to all
  vertices in that clique.
-}
extendingVerts :: Graph -> Clique -> [Vert]
extendingVerts g c = elems (intersections (Prelude.map (g !!) (elems c)))


{-
  Given a graph g, a clique c, and a history h, find all vertices that extend c.

  A vertex that extends a clique is a vertex that is adjacent to all
  vertices in that clique.

  A history is a set of vertices that are adjacent to all vertices in a clique but
  the maximum valued vertex.
-}
extendingVertsH :: Graph -> (Clique, Set Vert) -> Set Vert
extendingVertsH g (c, h) = intersection (g !! (findMax c)) h


{-
  Given a graph g and a list of cliques s, find all cliques in g that
  contain a clique from s and one additional vertex.
-}
biggerCliques :: Graph -> [Clique] -> [Clique]
biggerCliques g s = [insert w y | y <- s, w <- extendingVerts g y]


{-
  Given a graph g and a list of pairs of cliques and histories d, find all cliques in
  g that contain a clique from d and one additional vertex.
-}
biggerCliquesH :: Graph -> [(Clique, Set Vert)] -> [(Clique, Set Vert)]
biggerCliquesH g d = [(insert v (fst p), (extendingVertsH g p)) | p <- d, v <- elems (extendingVertsH g p)]



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
  Given a graph g, find all cliques of size n (using history).
-}
cliquesH' :: Graph -> Int -> [(Clique, Set Vert)]
cliquesH' g 0  = [(empty, allVerts g)]
cliquesH' g n  = (iterate (biggerCliquesH g) [(empty, allVerts g)]) !! n
cliquesH :: Graph -> Int -> [Clique]
cliquesH g n = Prelude.map fst (cliquesH' g n)


{-
  Given a graph g, find all cliques of size n that
  include vertex 0.
-}
rootcliques :: Graph -> Int -> [Clique]
rootcliques g 0 = [empty]
rootcliques g n = (iterate (biggerCliques g) [singleton 0]) !! (n - 1)


{-
  Given a graph g, find all cliques of size n that
  include vertex 0 (using history).
-}
rootcliquesH' :: Graph -> Int -> [(Clique, Set Vert)]
rootcliquesH' g 0 = [(empty, allVerts g)]
rootcliquesH' g n = (iterate (biggerCliquesH g) [(singleton 0, g !! 0)]) !! (n - 1)
rootcliquesH :: Graph -> Int -> [Clique]
rootcliquesH g n = Prelude.map fst (rootcliquesH' g n)


