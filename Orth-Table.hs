{-
  Generate a lookup table indicating which vectors are orthogonal
  to which other vectors.  This can be thought of as the adjacency
  matrix (in sparse form) of a graph describing orthogonality between
  vectors.  Orthonormal bases are maximal cliques on this graph.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}


import Data.List
import Data.Binary
import Ix
import System.IO
import System(getArgs)


{-
  Dimension we're working in.
-}
d :: Int
d = 6
n :: Int
n = 12
m :: Int
m = n^(d - 1)


{-
  Compute a vector's magic number.
-}
vec2magic :: [Int] -> Int
vec2magic []        = 0
vec2magic (xh : xt) = xh * n^(d - (length xt) - 2) + (vec2magic xt)


{-
  Compute a magic number's vector.
-}
magic2vec' x y = mod (div x (n^y)) n
magic2vec :: Int -> [Int]
magic2vec x = map (magic2vec' x) (range(0, d - 2))


{-
  Add two vectors.
-}
addVecs :: [Int] -> [Int] -> [Int]
addVecs a b = zipWith (\x y -> mod (x + y) (length a)) a b


{-
  Adjust a vector with an offset vector from a LUT.
-}
adjustVec :: [Int] -> [Int] -> [Int]
adjustVec a b = zipWith (\x y -> mod (x - y) n) a b


{-
  All indices of orthogonal entries in a lookup table.
-}
allOrthIndices lut = findIndices (\x -> (x == [True, False]) || (x == [True, True])) lut


{-
  All vectors corresponding to all orthogonal entries in
  lookup table.
-}
allOrthOffsets lut = map magic2vec (allOrthIndices lut)


{-
  Magic numbers for all vectors orthogonal to a particular vector.
-}
allOrthVecs lut x = map (\y -> vec2magic (adjustVec x y)) (allOrthOffsets lut)


{-
  Great big table of orthogonality.
-}
orthLUT lut = map (\x -> allOrthVecs lut (magic2vec x)) (range (0, m - 1))


{-
  ./Orth-Table [in] [out]

  in:  vector orthogonality, bias lookup table filename
  out: target filename for orthogonal pairs lookup table
-}
main = do
  arg0 : (arg1 : argT) <- getArgs
  lut <- decodeFile  arg0 :: IO [[Bool]]

  putStr ((show $ m) ++ " nodes.\n")
  putStr ((show $ length (allOrthIndices lut)) ++ " edges per node.\n")
  putStr ((show $ m * (length (allOrthIndices lut))) ++ " edges total.\n")

  encodeFile arg1 (orthLUT lut)


