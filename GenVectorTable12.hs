{-
  Generate table of inner products and bias values (against the unity
  vector) for all d-dimensional vectors consisting of 12th roots of unity.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module GenVectorTable12 where

import Data.List
import Data.Binary
import System.IO

import ExtRat
import ExtCpx
import Roots12


{-
  Dimension we're working in.
-}
d :: Integer
d = 6
n :: Integer
n = 12


{-
  Type definition for vectors.
-}
type HadV = [Integer]


{-
  Compute the inner product of two vectors.
-}
ip :: HadV -> HadV -> ExtCpx
ip [] [] = 1
ip (xh : xt) (yh : yt) = (roots !! (fromInteger xh))
                       * (roots !! (fromInteger yh))
                       + (ip xt yt)


{-
  Combine the orthogonal-to-unity and unbiased-to-unity information
  into a list for a particular vector.
-}
vec_stat :: HadV -> [Bool]
vec_stat x = [ (ip x unity_v) == 0,
               (abs (ip x unity_v)) - (fromInteger d) == 0 ]


{-
  The largest vector.
-}
max_v :: HadV
max_v = take (fromInteger (d - 1)) (repeat (n - 1))


{-
  The smallest vector, the unity vector.
-}
unity_v :: HadV
unity_v = take (fromInteger (d - 1)) (repeat 0)


{-
  Determine if a vector is the largest vector.
-}
is_max :: HadV -> Bool
is_max = (== max_v)


{-
  Compute the next vector in the sequence.
-}
next_v :: HadV -> HadV 
next_v [] = [1]
next_v (x : xs) = if x < n - 1
                then (x + 1):xs
                else 0 : next_v xs


{-
  All possible vectors.
-}
all_vecs :: [HadV]
all_vecs = all_vecs' unity_v
           where all_vecs' v | not $ is_max v = v : all_vecs' (next_v v)
                             | otherwise = v : []


{-
  Orthogonality and unbiasedness (to unity) for all vectors.
-}
vec_table :: [[Bool]]
vec_table = map vec_stat all_vecs

