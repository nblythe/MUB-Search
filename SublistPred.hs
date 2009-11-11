{-
  Find solutions to the "sublist-predicate problem".

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  The elements in a list x form a set.  The sublist-predicate problem is to find all
  length n sublists of this set that satisfy a predicate p.

  By "sublists" we mean that the solution lists may contain duplicate elements from x
  but the total set of solutions will not contain permutations.
-}

module SublistPred (sublists, sublistCount, sublistJobs, sublistPredP, sublistPred) where

import Combinadics
import Data.List


{-
  The number of length m sublists, not allowing permutations, of a list x.
-}
sublistCount m x = numRecombs (toInteger $ length x) m


{-
  All length m sublists of the set of elements in a list x, not allowing permutations, split
  into lists of j elements each.
-}
sublists x m j = map f [0 .. div c j]
                 where n   = toInteger $ length x
                       f s = map (select x) $ makeRecombs n m (s * j) (l s)
                       c   = numRecombs n m
                       l s = (min ((s * j) + j - 1) (c - 1)) - (s * j) + 1


{-
  The number of jobs into which the set of sublists will be partitioned.
-}
sublistJobs m x s = div (numRecombs (toInteger $ length x) m) s


{-
  All length m sublists of the set of elements in a list x, not allowing permutations, that satisfy a predicate p.

  This function splits the workload into lists of s sublists.
-}
sublistPredP p m x s = map (\ j -> filter p $ genericIndex (sublists x m s) j) [0 .. (sublistJobs m x s) - 1]


{-
  All length n sublists of the set of elements in a list x, not allowing permutations, that satisfy a predicate p.

  This function does not allow access to the results of individual workloads.
-}
sublistPred p m x = concat $ sublistPredP p m x (toInteger $ length x)

