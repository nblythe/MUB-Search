{-
  Find cliques on a Butson graph

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

module Graph2 (cliques) where

import Magic2
import Data.List


{-
  Compute the pointwise difference (mod n) between two vectors a and b.
-}
pointDiff :: Integer -> [Integer] -> [Integer] -> [Integer]
pointDiff n a b = zipWith (\x y -> mod (x - y) n) a b


{-
  A map followed by a filter, performed simultaneously.
-}
milter :: (a -> b) -> (b -> Bool) -> [a] -> [b]
milter _ _ [] = []
milter f p (h : t) = if   p (f h)
                     then f h : milter f p t
                     else milter f p t


{-
  The intersection of all lists in a list, performed pairwise repeatedly until
  finished.
-}
intersections :: (Eq a) => [[a]] -> [a]
intersections [] = []
intersections (h : []) = h
intersections l = intersections $ f l
                  where f [] = []
                        f (h : []) = [h]
                        f (h : g : t) = (intersect h g) : f t


{-
  The list of all vertices adjacent to and greater than a vertex v under a
  list l of fundamental adjacencies.
-}
neighbors :: (Integer, Integer) -> [Integer] -> Integer -> [Integer]
neighbors (d, n) l v = milter f (> v) l
                       where f a = vec2magic (d, n) (pointDiff n x y)
                                   where x = magic2vec (d, n) v
                                         y = magic2vec (d, n) a


{-
  All size m super cliques of a clique q, given a list p of potential
  extending vertices under a list l of fundamental adjacencies.
-}
cliques' :: (Integer, Integer) -> [Integer] -> ([Integer], [Integer]) -> Integer -> [[Integer]]
cliques' (_, _) _ (q, _) 0 = [q]
cliques' (d, n) l (q, p) m = concatMap (\ qp' -> cliques' (d, n) l qp' (m - 1) ) qp's
                          where qp's = map (\ h -> (h : q, intersect p (neighbors (d, n) l h)) ) p


{-
  The list of cliques of size m that include a clique from list qs, given a
  list l of fundamental adjacency relations.
-}
cliques :: (Integer, Integer) -> [Integer] -> Integer -> [[Integer]] -> [[Integer]]
cliques (d, n) l m qs = concatMap (\ q -> cliques' (d, n) l (f q) (m' q)) qs
                        where f q  = (q, intersections $ map (\ v -> neighbors (d, n) l v) q)
                              m' q = m - (toInteger $ length q)

