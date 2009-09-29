{-
  Find solutions to the "sublist-predicate problem".

  2009 Nathan Blythe, Dr. Oscar Boykin

  Given a list x, find all sublists of length n, not including permutations
  and which may include duplicates from x, such that a predicate p is
  satisfied for each sublist.
-}

module SublistPred (sublistPred) where


{-
  All length n sublists of a list x, with replacement but no permutations.
-}
sublists' x 1 = [[y] | y <- x]
sublists' x n = concat [[alpha : beta | beta <- sublists' (filter (>= alpha) x) (n - 1)] | alpha <- x]
sublists x n = map (map (x !!)) (sublists' [0 .. (length x) - 1] n)


{-
  All length n sublists of a list x, with replacement but no permutations,
  that satisfy a predicate p.
-}
sublistPred x n p = filter p (sublists x n)

