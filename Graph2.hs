{-
  Butson graphs: construction and clique-finding.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Graph2 (cliques, intersections) where

import Magic2
import Data.List (genericIndex, intersect)


{-
  Compute the pointwise difference (mod n) between two vectors a and b.
-}
pointDiff :: Integer -> [Integer] -> [Integer] -> [Integer]
pointDiff n a b = zipWith (\x y -> mod (x - y) n) a b


{-
  A map followed by a filter, but simultaneously.
-}
milter :: (a -> b) -> (b -> Bool) -> [a] -> [b]
milter _ _ [] = []
milter f p (h : t) = if   p (f h)
                     then f h : milter f p t
                     else milter f p t


{-
  The intersection of all lists in a list.
-}
intersections :: (Eq a) => [[a]] -> [a]
intersections (h : []) = h
intersections (h : g : []) = intersect h g
intersections l = intersections $ f l
                  where f [] = []
                        f (h : []) = [h]
                        f (h : g : t) = (intersect h g) : f t


{-
  The list of all vertices adjacent to and greater than a vertex v under a
  list l of fundamental adjacencies.
-}
neighbors :: (Integer, Integer) -> [Integer] -> Integer -> [Integer]
neighbors (d, n) l v = milter (f v) (> v) l
                       where f v a = vec2magic (d, n) (pointDiff n y x)
                                     where x = magic2vec (d, n) v
                                           y = magic2vec (d, n) a


{-
  The list of cliques that consist of cliques in a list z and one additional
  vertex, given a list l of fundamental adjacency relations.
-}
biggerCliques :: (Integer, Integer) -> [Integer] -> [[Integer]] -> [[Integer]]
biggerCliques (d, n) l z = [x : q | q <- z, x <- f q]
                           where f = intersections . (map (neighbors (d, n) l))


{-
  The list of cliques of size m that include clique q, given a list l of
  fundamental adjacency relations.
-}
growClique :: (Integer, Integer) -> [Integer] -> Integer -> [Integer] -> [[Integer]]
growClique (d, n) l m q = if   m == k
                          then [q]
                          else genericIndex qs (m - k)
                          where k  = toInteger $ length q
                                qs = iterate (biggerCliques (d, n) l) [q]


{-
  The list of cliques of size m that include a clique from list qs, given a
  list l of fundamental adjacency relations.
-}
cliques :: (Integer, Integer) -> [Integer] -> Integer -> [[Integer]] -> [[Integer]]
cliques (d, n) l m qs = concat $ map (growClique (d, n) l m) qs

