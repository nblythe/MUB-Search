module RootBalance (rootBalance) where

import Data.List
import Data.Maybe

import ExtRat
import ExtCpx


{-
  12th roots of unity.
-}
r0 = ExtRat 0   0 0 0             -- 0
r1 = ExtRat 0.5 0 0 0             -- 1/2
r2 = ExtRat 0   0 0.5 0           -- sqrt(3) / 2
r3 = ExtRat 1   0 0 0             -- 1

--                   Real   Imag     Root
roots12 = [ ExtCpx   r3     r0    -- 0
          , ExtCpx   r2     r1    -- 1
          , ExtCpx   r1     r2    -- 2
          , ExtCpx   r0     r3    -- 3
          , ExtCpx (-r1)    r2    -- 4
          , ExtCpx (-r2)    r1    -- 5
          , ExtCpx (-r3)    r0    -- 6
          , ExtCpx (-r2)  (-r1)   -- 7
          , ExtCpx (-r1)  (-r2)   -- 8
          , ExtCpx   r0   (-r3)   -- 9
          , ExtCpx   r1   (-r2)   -- 10
          , ExtCpx   r2   (-r1) ] -- 11

{-
  Convert a set of 12th roots of unity to a set of root numbers.
-}
roots2ints w = map (\x -> fromJust (findIndex (x ==) roots12)) w


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


{-
  All sets of n 12th roots of unity that sum to z.
-}
rootBalance n z = map roots2ints (subsetSum roots12 n z)

