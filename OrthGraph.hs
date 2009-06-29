{-
  Generate the adjacency matrix for a graph where vertices represent
  vectors and edges represent orthogonality.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

--module OrthGraph (orthGraph) where
module OrthGraph where

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
  Given a vector v, find the magic numbers of all vectors
  orthogonal to v.
-}
allOrthVecs2 (d, n) orthV v = [ vec2magic (d, n) (subVecs n v z) | z <- orthV]



{-
  Sparse adjacency matrix for an orthogonality graph.
-}
orthGraph (d, n) orthT = [ allOrthVecs (d, n) orthT x
                         | x <- take (n^(d - 1)) (iterate (1 +) 0)]




{-
  All vectors.
-}
nextVec (d, n) []        = [1]
nextVec (d, n) (xH : xT) = if   xH < n - 1
                           then (xH + 1) : xT
                           else 0 : nextVec (d, n) xT
allVecs (d, n) = take (n^(d - 1)) (iterate (nextVec (d, n)) (replicate d 0))



{-
  Sparse adjacency matrix for an orthogonality graph.
-}
--orthGraph2 (d, n) orthT = [ allOrthVecs2 (d, n) orthV x
--                          | x <- take (n^(d - 1)) (iterate (1 +) 0)]
--                          where orthV = map (magic2vec (d, n)) orthT
orthGraph2 (d, n) orthT = map (allOrthVecs2 (d, n) orthV) (allVecs (d, n))
                          where orthV = map (magic2vec (d, n)) orthT


orthGraphF (d, n) orthT = allOrthVecs (d, n) orthT
