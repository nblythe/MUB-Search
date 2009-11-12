{-
  Find cliques on a graph

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

module Cliques (cliques) where

import Data.List


type Eqp a = (a -> a -> Bool)
type Ngf a = (a -> [a])


{-
  The intersection of all lists in a list, performed pairwise repeatedly until
  finished, given an equivalence predicate.
-}
intersectionsBy :: Eqp a -> [[a]] -> [a]
intersectionsBy _ [] = []
intersectionsBy _ (h : []) = h
intersectionsBy p l = intersectionsBy p (f l)
                      where f [] = []
                            f (h : []) = [h]
                            f (h : g : t) = (intersectBy p h g) : f t


{-
  All size k super cliques of a clique q, given a list l of potential
  extending vertices.
-}
cliques' :: Eqp a -> Ngf a -> Integer -> ([a], [a]) -> [[a]]
cliques' _ _ 0 (q, _) = [q]
cliques' p n k (q, l) = concatMap (cliques' p n (k - 1)) s
                        where s = map (\ x -> (x : q, intersectBy p l (n x))) l


{-
  All size k cliques that include a clique from list qs.
-}
cliques :: Eqp a -> Ngf a -> Integer -> [[a]] -> [[a]]
cliques p n k qs = concatMap (\ q -> cliques' p n (k' q) (f q)) qs
                   where f q  = (q, intersectionsBy p (map n q))
                         k' q = k - (toInteger (length q))

