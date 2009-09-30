{-
  Find solutions to the "sublist-predicate problem".

  2009 Nathan Blythe, Dr. Oscar Boykin

  The elements in a list x form a set.  The sublist-predicate problem is to find all
  length n sublists of this set that satisfy a predicate p.

  By "sublists" we mean that the solution lists may contain duplicate elements from x
  but the total set of solutions will not contain permutations.
-}

module SublistPred (sublistPred) where


{-
  All length n ordered sublists of {k, k + 1, ... l}.
-}
osubsets k l 1 = [ [i] | i <- [k .. l] ]
osubsets k l n = concat $ map q [k .. l]
                 where q a = [ a : b | b <- osubsets a l (n - 1) ]


{-
  All length n sublists of the set of elements in a list x, not allowing permutations.
-}
sublists x n = map (map (x !!)) $ osubsets 0 (length x - 1) n


{-
  All length n sublists of the set of elements in a list x, not allowing permutations, that satisfy a predicate p.
-}
sublistPred p n x = filter p (sublists x n)

