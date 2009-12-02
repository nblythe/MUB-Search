{-
  Clique Finding on Vector Space Graphs
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
{-
  Type class for types that can be vertices on a vector space graph.

  e: equivalence predicate
  p: promote a vertex
  x: divide a promoted-vertex by another promoted-vertex
  q: demote a promoted-vertex
-}
class (Eq a, Ord a) => Vert a where
  e  :: a -> a -> Bool
  p  :: Integer -> Integer -> a -> [a]
  x  :: Integer -> Integer -> a -> a -> a
  q  :: Integer -> Integer -> [a] -> a


{-
  Integers are a valid vertex type.
-}
instance Vert Integer where
  e         = (==)
  p d n x   = magic2vec (d, n) x
  x _ n x y = mod (x - y) n
  q d n l   = vec2magic (d, n) l


{-
  Lists of Integers are a valid vertex type.
-}
instance (Vert [Integer]) where
  e  x y    = (sort x) == (sort y)
  p d n x   = magics2vecs (d, n) x
  x _ n x y = zipWith (\ x y -> mod (x - y) n) x y
  q d n l   = vecs2magics (d, n) l


{-
  The intersection of all lists in a list, performed pairwise repeatedly.
-}
ints :: (Vert a) => [[a]] -> [a]
ints []          = []
ints (h : [])    = h
ints (h : g : t) = ints ((intersectBy e h g) : t)


{-
  All size k super-cliques of a clique c, given a list r of potential
  extending promoted-vertices.
-}
cliques' :: (Vert a) => Integer -> Integer -> Integer -> [[a]] -> ([a], [a]) -> [[a]]
cliques' _ _ 0 l (c, _) = [c]
cliques' d n k l (c, r) = concatMap (cliques' d n (k - 1) l) s
                          where s = map (\ v -> (v : c, intersectBy e r (nbrs d n l v))) r


{-
  All size k cliques that include a clique from list cs.
-}
cliques :: (Vert a) => Integer -> Integer -> Integer -> [a] -> [[a]] -> [[a]]
cliques d n k l cs = concatMap (\ c -> cliques' d n (k' c) l' (g c)) cs
                     where g  c = (c, ints (map (nbrs d n l') c))
                           k' c = k - (toInteger . length) c
                           l'   = map (p d n) l


{-
  Neighbors to a vertex.
-}
nbrs :: Vert a => Integer -> Integer -> [[a]] -> a -> [a]
nbrs d n []      _ = []
nbrs d n (h : t) v = if    h' > v
                     then  h' : t'
                     else  t'
                     where h' = q d n (zipWith (x d n) (p d n v) h)
                           t' = nbrs d n t v

-}


{-
  Type class for scalars in the vector space underlying a vector space graph.

  e: equivalence predicate
  x: division
-}
class (Eq a, Ord a) => Scalar a where
  e  :: a -> a -> Bool
  x  :: Integer -> Integer -> a -> a -> a

type Vector a = [a]
type Clique a = [Vector a]


{-
  Integers are valid scalars.
-}
instance Scalar Integer where
  e         = (==)
  x _ n x y = mod (x - y) n


{-
  Lists of Integers are a valid scalars.
-}
instance (Scalar [Integer]) where
  e         = (==)
  x _ n x y = zipWith (\ x y -> mod (x - y) n) x y


{-
  Lists of lists of Integers are valid scalars.
-}
instance (Scalar [[Integer]]) where
  e  x y    = (sort x) == (sort y)
  x _ n x y = undefined


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
cliques' :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> Integer -> [Vector a] -> ([Vector a], [Vector a]) -> [Clique a]
cliques' _ _ 0 l (c, _) = [c]
cliques' d n k l (c, r) = concatMap (cliques' d n (k - 1) l) s
                          where s = map (\ v -> (v : c, intersectBy e r (nbrs d n l v))) r


{-
  All size k cliques that include a clique from list cs.
-}
cliques :: (Scalar a, Scalar (Vector a)) => Integer -> Integer -> Integer -> [Vector a] -> [Clique a] -> [Clique a]
cliques d n k l cs = concatMap (\ c -> cliques' d n (k' c) l (g c)) cs
                     where g  c = (c, ints (map (nbrs d n l) c))
                           k' c = k - (toInteger . length) c


{-
  Neighbors to a vertex.
-}
nbrs :: (Scalar a) => Integer -> Integer -> [Vector a] -> Vector a -> [Vector a]
nbrs d n []      _ = []
nbrs d n (h : t) v = if    h' > v
                     then  h' : t'
                     else  t'
                     where h' = zipWith (x d n) v h
                           t' = nbrs d n t v
--nbrs d n l v = map (zipWith (x d n) v) l

