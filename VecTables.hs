{-
  Tables of magic numbers for vectors orthogonal and unbiased
  to the unity vector.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module VecTables (vecTableOrth12, vecTableUnbias12, vecTableOrth24, vecTableUnbias24) where

import Roots12
import Roots24
import SubsetPred
import Magic


{-
  Orthogonality table for vectors in dimension 6 constructed from the
  12th roots of unity.
-}
vecTableOrth12 = map (vec2magic (6, 12))
                     (map Roots12.toInts
                          (subsetPred Roots12.roots 5 (\x -> 0 == 1 + (sum x))))


{-
  Unbiased table for vectors in dimension 6 constructed from the
  12th roots of unity.
-}
vecTableUnbias12 =  map (vec2magic (6, 12))
                        (map Roots12.toInts
                             (subsetPred Roots12.roots 5 (\x -> 6 == abs (1 + (sum x)))))


{-
  Orthogonality table for vectors in dimension 6 constructed from the
  24th roots of unity.
-}
vecTableOrth24 =  map (vec2magic (6, 24))
                      (map Roots24.toInts
                           (subsetPred Roots24.roots 5 (\x -> 0 == 1 + (sum x))))


{-
  Unbiased table for vectors in dimension 6 constructed from the
  24th roots of unity.
-}
vecTableUnbias24 =  map (vec2magic (6, 24))
                        (map Roots24.toInts
                             (subsetPred Roots24.roots 5 (\x -> 6 == abs (1 + (sum x)))))


