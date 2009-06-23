{-
  Generate the adjacency matrix for a graph where vertices represent
  vectors and edges represent orthogonality.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module GenOrthGraph where

import Data.List
import Data.Binary
import Ix
import System.IO


{-
  Compute a vector's magic number.
-}
vec2magic (d, n) []        = 0
vec2magic (d, n) (xh : xt) = xh * n^(d - (length xt) - 2) + (vec2magic (d, n) xt)


{-
  Compute a magic number's vector.
-}
magic2vec' (d, n) x y = mod (div x (n^y)) n
magic2vec  (d, n) x   = map (magic2vec' (d, n) x) (range(0, d - 2))


{-
  Adjust a vector with an offset vector from a LUT.
-}
adjustVec (d, n) a b = zipWith (\x y -> mod (x - y) n) a b


{-
  All indices of orthogonal entries in a lookup table.
-}
allOrthIndices lut = findIndices (\x -> (x == [True, False]) || (x == [True, True])) lut


{-
  All vectors corresponding to all orthogonal entries in
  lookup table.
-}
allOrthOffsets (d, n) lut = map (magic2vec (d, n)) (allOrthIndices lut)


{-
  Magic numbers for all vectors orthogonal to a particular vector.
-}
allOrthVecs (d, n) lut x = map (\y -> vec2magic (d, n) (adjustVec (d, n) x y)) (allOrthOffsets (d, n) lut)


{-
  Great big table of orthogonality.
-}
orthLUT (d, n) lut = map (\x -> allOrthVecs (d, n) lut (magic2vec (d, n) x)) (range (0, (n^(d - 1)) - 1))

