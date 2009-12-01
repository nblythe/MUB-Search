{-
  A bijection between vectors of dimension d in n variables and natural
  numbers 0 .. n^d - 1.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

module Magic (vec2magic, vecs2magics, magic2vec, magics2vecs) where

import Data.List (genericTake)


{-
  Compute a vector's magic number.
-}
vec2magic :: (Integer, Integer) -> [Integer] -> Integer
{-vec2magic (d, n) []        = 0
vec2magic (d, n) (xh : xt) =   xh * n^(d - (toInteger $ length xt) - 2)
                             + (vec2magic (d, n) xt)-}

vec2magic (d, n) x = foldl (\ a (h, l) -> a + h * n^l) 0 $ zip x [0 .. (toInteger $ length x) - 1]


{-
  Convert a list of vectors to a list of magic numbers.
-}
vecs2magics :: (Integer, Integer) -> [[Integer]] -> [Integer]
vecs2magics (d, n) l = map (vec2magic (d, n)) l


{-
  Compute a magic number's vector.
-}
magic2vec :: (Integer, Integer) -> Integer -> [Integer]
magic2vec (d, n) x = genericTake (d - 1) $ Prelude.map (\y -> mod y n) allDivs
                     where allDivs = (iterate (\y -> div y n) x)


{-
  Convert a list of magic numbers to a list of vectors.
-}
magics2vecs :: (Integer, Integer) -> [Integer] -> [[Integer]]
magics2vecs (d, n) l = map (magic2vec (d, n)) l

