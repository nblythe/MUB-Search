{-
  Find solutions to a variant of the subset sum problem with
  replacement and permutations.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module SubsetSum (subsetSum) where


{-
  All length n subsets of a set x, with replacement and permutations.
-}
subsets x 1 = [[y] | y <- x]
subsets x n = [y : w | y <- x, w <- subsets x (n - 1)]


{-
  All length n subsets of a set x, with replacement and permutations,
  that sum to z.
-}
subsetSum x n z = filter (\y -> z == sum y) (subsets x n)

