{-
  Mappings from vectors of dimension d (where each element is an integer 0 -> n - 1)
  integers and vice versa.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

--module Magic2 (vec2magic, vecs2magics, magic2vec, magics2vecs) where
module Magic2 (vec2magic, magic2vec) where

--import Data.Set
import Data.List (genericTake)


{-
  Compute a vector's magic number.
-}
vec2magic :: (Integer, Integer) -> [Integer] -> Integer
vec2magic (d, n) []        = 0
vec2magic (d, n) (xh : xt) = xh * n^(d - (toInteger $ length xt) - 2) + (vec2magic (d, n) xt)


{-
  Convert a set of vectors to a set of magic numbers.
-}
--vecs2magics (d, n) s = Data.Set.map (vec2magic (d, n)) s


{-
  Compute a magic number's vector.
-}
magic2vec :: (Integer, Integer) -> Integer -> [Integer]
magic2vec (d, n) x = genericTake (d - 1) $ Prelude.map (\y -> mod y n) allDivs
                     where allDivs = (iterate (\y -> div y n) x)



{-
  Convert a set of magic numbers to a set of vectors.
-}
--magics2vecs (d, n) s = Data.Set.map (magic2vec (d, n)) s

