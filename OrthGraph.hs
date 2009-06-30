{-
  Generate the adjacency matrix for a graph where vertices represent
  vectors and edges represent orthogonality.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module OrthGraph (orthGraph) where

import Magic


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
orthGraph (d, n) orthT = [ allOrthVecs (d, n) orthT x | x <- take (n^(d - 1)) (iterate (1 +) 0)]

