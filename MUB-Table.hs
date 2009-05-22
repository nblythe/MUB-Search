import Data.List
import Data.Complex
import Data.Array
import System.IO


{-
  Dimension we're working in.
-}
d :: Int
d = 6


{-
  Will construct vectors with elements selected from the
  nth roots of unity.
-}
n :: Int
n = 12


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
vec_stat :: HadV -> [Float]
vec_stat x = [ abs((magnitude (ip x unity_v))),
               abs((magnitude (ip x unity_v)) - sqrt(fromIntegral d)) ]


{-
  The largest Hadamard vector.
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
vec_table :: [[Float]]
vec_table = map vec_stat all_vecs


{-
  Convert a double into a string containing
  two comma seperated variables.
-}
csv_vec :: [Float] -> String
csv_vec [a, b] = (show(a) ++ "," ++ show(b) ++ "\n")


{-
  Convert a table of doubles into a string containing
  a comma seperated list.
-}
csv_table :: [[Float]] -> String
csv_table [] = ""
csv_table (h : t) = csv_vec(h) ++ csv_table(t)


main = do writeFile "temp" (csv_table vec_table)
  --putStr ("Have " ++ (show $ length all_vecs) ++ " vectors.\n\n")
  --putStr ("And here they are: " ++ (show all_vecs) ++ "\n")

