{-
  Find cliques on a graph

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

module Voodoo where


{-
  Permutation-free list, given a function to compute a permutation-invariant of
  an object.
-}
permfree :: (Eq b) => (a -> b) -> [a] -> [a]
permfree f l = g [] l
               where g _ [] = []
                     g sl (h : t) = if   all (/= h') sl
                                    then h : g (h' : sl) t
                                    else g sl t
                                    where h' = f h


{-
  A map followed by a filter, performed simultaneously.
-}
milter :: (a -> b) -> (b -> Bool) -> [a] -> [b]
milter _ _ [] = []
milter f p (h : t) = if   p (f h)
                     then f h : milter f p t
                     else milter f p t


{-
  Given a function e to expand an object, a function x to combine two
  expanded objects, a function c to contract an object, and a list l of
  objects adjacent to the 0 object, compute the list of objects adjacent
  to an object v.
-}
neighbors :: Ord a => (a -> [a]) -> ([a] -> [a] -> [a]) -> ([a] -> a) -> [a] -> a -> [a]
neighbors e x c l v = milter f (> v) l
                      where f a = c (x (e v) (e a))

