{-
  Generate table of inner products and bias values (against the unity
  vector) for all d-dimensional vectors consisting of 12th roots of unity.

  2009 Dr. Oscar Boykin, Nathan Blythe

  Floating point version.
-}

import Data.List
import Data.Complex
import Data.Array
import Data.Binary
import System.IO
import System(getArgs)


{-
  Dimension we're working in.
-}
d :: Int
d = 6
n :: Int
n = 12


{-
  Threshold for "tiny" numbers.
-}
tiny :: Float
tiny = 0.0001


{-
  Type definition for vectors.
-}
type HadV = [Int]


{-
  Compute the kth nth root of unity.
  (If n = 6, "int_to_comp 2" computes the second 6th root of unity)
-}
int_to_comp :: Int -> Complex Float
int_to_comp k = cis $ 2.0 * pi * (fromIntegral k) / (fromIntegral n)


{-
  All nth roots of unity.
-}
int_to_comp_a = listArray (-n, n) [ int_to_comp k | k <- range (-n, n)]


{-
  Compute the inner product of two vectors.
-}
ip :: HadV -> HadV -> Complex Float
ip xs ys = (sum $ map (int_to_comp_a !) [ x - y | (x,y) <- zip xs ys ]) + 1


{-
  Combine the orthogonal-to-unity and unbiased-to-unity information
  into a list for a particular vector.
-}
vec_stat :: HadV -> [Bool]
vec_stat x = [ abs((magnitude (ip x unity_v) ^ 2)) < tiny,
               abs((magnitude (ip x unity_v) ^ 2) - (fromIntegral d)) < tiny ]


{-
  The largest vector.
-}
max_v :: HadV
max_v = take (d - 1) (repeat (n - 1))


{-
  The smallest vector, the unity vector.
-}
unity_v :: HadV
unity_v = take (d - 1) (repeat 0)


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


{-
  Convert a double into a string containing
  two comma seperated variables.
-}
csv_vec :: [Bool] -> String
csv_vec [a, b] = (show(a) ++ "," ++ show(b) ++ "\n")


{-
  Convert a table of doubles into a string containing
  a comma seperated list.
-}
csv_table :: [[Bool]] -> String
csv_table [] = ""
csv_table (h : t) = csv_vec(h) ++ csv_table(t)


{-
  Convert a boolean to a 0 or 1.
-}
bool2bit :: Bool -> String
bool2bit x = case x of
                  True -> "1"
                  False -> "0"


{-
  Convert a list of booleans to a string of 0s and 1s seperated
  by spaces.
-}
bools2bits :: [Bool] -> String
bools2bits []        = ""
bools2bits (xh : []) = bool2bit xh
bools2bits (xh : xt) = (bool2bit xh) ++ " " ++ (bools2bits xt)


{-
  Convert a list of boolean lists into a string containing
  lines of 0s and 1s.
-}
lbools2bits :: [[Bool]] -> String
lbools2bits []        = ""
lbools2bits (xh : []) = bools2bits xh
lbools2bits (xh : xt) = (bools2bits xh) ++ "\n" ++ (lbools2bits xt)


{-
  ./MUB-Table-exact outfile
-}
main = do
  argH0 : (argH1 : argT) <- getArgs

  let orthD = map head vec_table
  let biasD = map (\x -> head (tail x)) vec_table

  let orthT = findIndices (== True) orthD
  let biasT = findIndices (== True) biasD

  encodeFile argH0 orthT
  encodeFile argH1 biasT

