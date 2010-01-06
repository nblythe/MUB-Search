{-
  Clique Finding on Vector Space Graphs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  TODO

  Note: the "Flexible Instances" and "Flexible Constructs" Haskell extensions
  are required.  In GHC/GHCI this necessitates the -XFlexibleInstances and
  -XFlexibleConstructs flags.
-}

module Cliques (cliques) where

import Data.List


{-
  Type class for scalars in the vector space underlying a vector space graph.

  e: equivalence predicate
  x: division
-}
class (Eq a, Ord a) => Scalar a where
  e  :: a -> a -> Bool
  x  :: Integer -> a -> a -> a


{-
  A vector is a list of scalars.
-}
type Vector a = [a]


{-
  A clique is a list of vectors.

  Most lists of vectors are not cliques, of course, but cliques are typed as
  lists of vectors.
-}
type Clique a = [Vector a]


{-
  Scalar field: any finite subset of consecutive integers.

  Equivalence: exact equality.
  Division:    subtraction modulo the size of the scalar field.
-}
instance Scalar Integer where
  e       = (==)
  x n x y = mod (x - y) n


{-
  Scalar field: fixed-length lists of integers over any finite subset of
  consecutive integers.

  Equivalence: exact equality.
  Division:    pointwise subtraction modulo the size of the underlying integer field.
-}
instance (Scalar [Integer]) where
  e       = (==)
  x n x y = zipWith (\ x y -> mod (x - y) n) x y


{-
  Scalar field: fixed-length lists of fixed-length lists over any finite subset
  of consecutive integers.

  Equivalence: permutations.
  Division:    undefined.
-}
instance (Scalar [[Integer]]) where
  e   x y = (sort x) == (sort y)
  x n x y = undefined


{-
  The intersection of all lists in a list.

  This is a reasonably fast implementation, which takes the pairwise
  intersections repeatedly until finished.

  Ex:  [ [1, 2, 6], [2, 6, 7], [2, 5, 6], [2, 5, 6], [2, 6, 8] ]
      ->  [ [2, 6], [2, 5, 6], [2, 6, 8] ]
      ->  [ [2, 6], [2, 6, 8] ]
      ->  [ [2, 6] ]
      ->  [2, 6]
-}
ints :: (Scalar a) => [[a]] -> [a]
ints []          = []
ints (h : [])    = h
ints (h : g : t) = ints ((intersectBy e h g) : t)


{-
  All size k super-cliques of a clique c, given a list r of potential
  extending vertices.

  Extending vertices are vertices that have been vetted as being neighbors to
  all vertices in c.  That is, any vertex in the list of extending vertices can
  be added to c to increase the size of the clique by one.

  The idea is to form all the extended cliques (all cliques formed by adding
  one of the extending vertices to c) and then find the lists of extending
  vertices for each such extended clique, and recurse until k = 0, collecting
  everything as we go.

  n is the size of the underlying scalar field.
  l is the list of adjacencies (see nbrs).
-}
cliques' :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [Vector a] -> ([Vector a], [Vector a]) -> [Clique a]
cliques' _ 0 _ (c, _) = [c]
cliques' n k l (c, r) = concatMap (cliques' n (k - 1) l) s
                        where s = map (\ v -> (v : c, intersectBy e r (nbrs n l v))) r


{-
  All size k super-cliques of at least one clique from list cs.

  Essentially we are just wrapping cliques' for each clique in cs.

  n is the size of the underlying scalar field.
  l is the list of adjacencies (see nbrs).
-}
cliques :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [Vector a] -> [Clique a] -> [Clique a]
cliques n k l cs = concatMap (\ c -> cliques' n (k' c) l (g c)) cs
                    where g  c = (c, ints (map (nbrs n l) c))
                          k' c = k - (toInteger . length) c


{-
  Neighbors to a vertex.

  Given a list (h : t) of adjacencies (vertices that are adjacenct to the
  origin vertex) we can find the list of vertices adjacent to any particular
  vertex, such as v.

  This is the underlying concept behind Vector Space Graphs.
-}
nbrs :: (Scalar a) => Integer -> [Vector a] -> Vector a -> [Vector a]
nbrs n []      _ = []
nbrs n (h : t) v = if    h' > v
                   then  h' : t'
                   else  t'
                   where h' = zipWith (x n) v h
                         t' = nbrs n t v

