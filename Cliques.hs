{-
  Clique Finding on Vector Space Graphs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  Butson graphs are graphs in which vertices are tensors constructed from nth
  roots of unity and adjacencies are binary tensor relations.

  Vertex types are defined for Integers (scalars) and lists of Integers
  (vectors).

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

type Vector a = [a]
type Clique a = [Vector a]


{-
  Integers are valid scalars.
-}
instance Scalar Integer where
  e       = (==)
  x n x y = mod (x - y) n


{-
  Lists of Integers are a valid scalars.
-}
instance (Scalar [Integer]) where
  e       = (==)
  x n x y = zipWith (\ x y -> mod (x - y) n) x y


{-
  Lists of lists of Integers are valid scalars.
-}
instance (Scalar [[Integer]]) where
  e   x y = (sort x) == (sort y)
  x n x y = undefined


{-
  The intersection of all lists in a list, performed pairwise repeatedly.
-}
ints :: (Scalar a) => [[a]] -> [a]
ints []          = []
ints (h : [])    = h
ints (h : g : t) = ints ((intersectBy e h g) : t)


{-
  All size k super-cliques of a clique c, given a list r of potential
  extending vertices.
-}
cliques' :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [[Vector a]] -> ([Vector a], [Vector a]) -> [Clique a]
cliques' _ 0 _ (c, _) = [c]
cliques' n k (l : ls) (c, r) = concatMap (cliques' n (k - 1) ls) s
                               where s = map (\ v -> (v : c, intersectBy e r (nbrs n l v))) r


{-
  All size k cliques that include a clique from list cs.
-}
cliques :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> [[Vector a]] -> [Clique a] -> [Clique a]
cliques n k (l : ls) cs = concatMap (\ c -> cliques' n (k' c) ls (g c)) cs
                          where g  c = (c, ints (map (nbrs n l) c))
                                k' c = k - (toInteger . length) c


{-
  Neighbors to a vertex.
-}
nbrs :: (Scalar a) => Integer -> [Vector a] -> Vector a -> [Vector a]
nbrs n []      _ = []
nbrs n (h : t) v = if    h' > v
                   then  h' : t'
                   else  t'
                   where h' = zipWith (x n) v h
                         t' = nbrs n t v

