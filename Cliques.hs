{-
  Clique Finding
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  Find cliques on a special graph, defined by the following features:
    - Vertices that can be "expanded" and "contracted" to lists of the same
      type as vertices themselves.

    - The vertices adjacent to the 0 vertex are sufficient to describe all
      edges on the graph.  TODO

    - The neighbors to a particular vertex can be generated by expanding the
      vertex and contracting its pointwise quotient with the (expanded)
      0-adjacent vertices.  TODO

  Vertex types are defined for Integers (scalars) and lists of Integers
  (vectors).

  Note: the "Flexible Instances" Haskell extension is required.  In GHC/GHCI
  this necessitates the -XFlexibleInstances flag.
-}

module Cliques (cliques) where

import Data.List

import Magic


{-
  Type class for types that can be vertices on a graph.

  eq: equivalence predicate
  e:  expand a list of vertices
  x:  divide a vertex by another vertex
  c:  contract a list of vertices
-}
class (Eq a, Ord a) => V a where
  eq :: a -> a -> Bool
  e  :: Integer -> Integer -> a -> [a]
  x  :: Integer -> Integer -> a -> a -> a
  c  :: Integer -> Integer -> [a] -> a


{-
  Integers are a valid vertex type.
-}
instance V Integer where
  eq        = (==)
  e d n x   = magic2vec (d, n) x
  x _ n x y = mod (x - y) n
  c d n l   = vec2magic (d, n) l


{-
  Lists of Integers are a valid vertex type.
-}
instance (V [Integer]) where
  eq x y    = (sort x) == (sort y)
  e d n x   = magics2vecs (d, n) x
  x _ n x y = zipWith (\ x y -> mod (x - y) n) x y
  c d n l   = vecs2magics (d, n) l


{-
  The intersection of all lists in a list, performed pairwise repeatedly.
-}
ints :: (V a) => [[a]] -> [a]
ints []          = []
ints (h : [])    = h
ints (h : g : t) = ints ((intersectBy eq h g) : t)


{-
  All size k super cliques of a clique q, given a list l of potential
  extending vertices.
-}
cliques' :: (V a) => Integer -> Integer -> Integer -> [a] -> ([a], [a]) -> [[a]]
cliques' _ _ 0 l (q, _) = [q]
cliques' d n k l (q, r) = concatMap (cliques' d n (k - 1) l) s
                      where s = map (\ v -> (v : q, intersectBy eq r (nbrs d n l v))) r


{-
  All size k cliques that include a clique from list qs.
-}
cliques :: (V a) => Integer -> Integer -> Integer -> [a] -> [[a]] -> [[a]]
cliques d n k l qs = concatMap (\ q -> cliques' d n (k' q) l (g q)) qs
                     where g  q = (q, ints (map (nbrs d n l) q))
                           k' q = k - (toInteger . length) q


{-
  Neighbors to a vertex.
-}
nbrs :: V a => Integer -> Integer -> [a] -> a -> [a]
nbrs d n []      _ = []
nbrs d n (h : t) v = if    h' > v
                     then  h' : t'
                     else  t'
                     where h' = c d n (zipWith (x d n) (e d n v) (e d n h))
                           t' = nbrs d n t v

