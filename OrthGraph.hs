{-
  Generate an adjacency structure for a graph where vertices represent
  vectors and edges represent orthogonality.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module OrthGraph (orthGraph, orthGraphD) where

import Magic
import Data.Set

{-
  Given two vectors a and b, compute a - b (mod n).
-}
subVecs n a b = zipWith (\x y -> mod (x - y) n) a b


{-
  Given a magic number x, find the magic numbers of all vectors
  orthogonal to the vector x represents.
-}
allOrthVecs (d, n) orthT x = [ vec2magic (d, n) (subVecs n vx (magic2vec (d, n) z))
                             | z <- orthT]
                             where vx = magic2vec (d, n) x


{-
  Sparse adjacency matrix for an orthogonality graph.
-}
orthGraph (d, n) orthT = [ Data.Set.fromList (allOrthVecs (d, n) orthT x) | x <- take (n^(d - 1)) [0..]]

{-
  Sparse adjacency matrix for an orthogonality graph, where graph edges
  are directed from lower-valued vertices to higher-valued vertices.
-}
orthGraphD (d, n) orthT = [ Data.Set.filter (> x) (Data.Set.fromList (allOrthVecs (d, n) orthT x)) | x <- take (n^(d - 1)) [0..]]

