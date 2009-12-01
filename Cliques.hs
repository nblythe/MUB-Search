{-
  Clique Finding on Butson Graphs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  Butson graphs are graphs in which vertices are tensors constructed from nth
  roots of unity and adjacencies are binary tensor relations.

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

  e: equivalence predicate
  p: promote a vertex
  x: divide a promoted-vertex by another promoted-vertex
  q: demote a promoted-vertexv
-}
class (Eq a, Ord a) => V a where
  e  :: a -> a -> Bool
  p  :: Integer -> Integer -> a -> [a]
  x  :: Integer -> Integer -> a -> a -> a
  q  :: Integer -> Integer -> [a] -> a


{-
  Integers are a valid vertex type.
-}
instance V Integer where
  e         = (==)
  p d n x   = magic2vec (d, n) x
  x _ n x y = mod (x - y) n
  q d n l   = vec2magic (d, n) l


{-
  Lists of Integers are a valid vertex type.
-}
instance (V [Integer]) where
  e  x y    = (sort x) == (sort y)
  p d n x   = magics2vecs (d, n) x
  x _ n x y = zipWith (\ x y -> mod (x - y) n) x y
  q d n l   = vecs2magics (d, n) l


{-
  The intersection of all lists in a list, performed pairwise repeatedly.
-}
ints :: (V a) => [[a]] -> [a]
ints []          = []
ints (h : [])    = h
ints (h : g : t) = ints ((intersectBy e h g) : t)


{-
  All size k super-cliques of a clique c, given a list r of potential
  extending promoted-vertices.
-}
cliques' :: (V a) => Integer -> Integer -> Integer -> [[a]] -> ([a], [a]) -> [[a]]
cliques' _ _ 0 l (c, _) = [c]
cliques' d n k l (c, r) = concatMap (cliques' d n (k - 1) l) s
                          where s = map (\ v -> (v : c, intersectBy e r (nbrs d n l v))) r


{-
  All size k cliques that include a clique from list cs.
-}
cliques :: (V a) => Integer -> Integer -> Integer -> [a] -> [[a]] -> [[a]]
cliques d n k l cs = concatMap (\ c -> cliques' d n (k' c) l' (g c)) cs
                     where g  c = (c, ints (map (nbrs d n l') c))
                           k' c = k - (toInteger . length) c
                           l'   = map (p d n) l


{-
  Neighbors to a vertex.
-}
nbrs :: V a => Integer -> Integer -> [[a]] -> a -> [a]
nbrs d n []      _ = []
nbrs d n (h : t) v = if    h' > v
                     then  h' : t'
                     else  t'
                     where h' = q d n (zipWith (x d n) (p d n v) h)
                           t' = nbrs d n t v

