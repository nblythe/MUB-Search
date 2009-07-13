{-
  Find solutions to the subset-predicate problem with replacement and
  permutations, and fixed subset size.

  2009 Nathan Blythe, Dr. Oscar Boykin

  Given a set x, find all subsets of length n, including permutations
  and which may include duplicates from x, such that a predicate p is
  satisfied for each subset.

  If x is the set of mth roots of unity then
    subsetPred x (d - 1) (\x -> 0 == 1 + (sum x))
  will find all vectors in dimension d constructed of mth roots of
  unity with first element 1 that are orthogonal to the unity vector.

  If x is the set of mth roots of unity then
    subsetPred x (d - 1) (\x -> d == absq (1 + (sum x)))
  will find all vectors in dimension d constructed of mth roots of
  unity with first element 1 that are unbiased to the unity vector,
  assuming that absq is a function that computes the absolute value
  of the square of a complex number.
-}

module SubsetPred (subsetPred) where

import Data.Set


{-
  All length n subsets of a set x, with replacement and permutations.
-}
--subsets x 1 = [[y] | y <- x]
--subsets x n = [y : w | y <- x, w <- subsets x (n - 1)]
subsets :: (Ord t) => Set t -> Int -> Set (Set t)
subsets x 1 = Data.Set.map singleton x
subsets x n = unions $ Prelude.map (\ y -> Data.Set.map (\ s -> insert y s) (subsets x (n - 1))) (elems x)


{-
  All length n subsets of a set x, with replacement and permutations,
  that satisfy a predicate p.
-}
--subsetPred x n p = filter p (subsets x n)
subsetPred :: (Ord t) => Set t -> Int -> (Set t -> Bool) -> Set (Set t)
subsetPred x n p = Data.Set.filter p (subsets x n)
