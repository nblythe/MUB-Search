{-
  Generate table of inner products and bias values (against the unity
  vector) for all d-dimensional vectors consisting of 12th roots of unity.

  2009 Dr. Oscar Boykin, Nathan Blythe

  Exact version.  File is CSV.
-}

import Data.List
import Data.Complex
import Data.Array
import System.IO
import System(getArgs)


{-
  Dimension we're working in.
-}
d :: Int
d = 3
n :: Int
n = 12


{-
  Extended rationals.
-}
data ExtRat = ExtRat Rational Rational

instance Num ExtRat where
  (ExtRat a1 b1) + (ExtRat a2 b2) = ExtRat (a1 + a2) (b1 + b2)
  (ExtRat a1 b1) * (ExtRat a2 b2) = ExtRat (a1 * a2 + b1 * b2 * 3 / 4) (a1 * b2 + a2 * b1)
  negate (ExtRat a b) = ExtRat (negate a) (negate b)
  abs (ExtRat a b) = error "abs undefined for extended rationals"
  signum (ExtRat a b) = error "signum undefined for extended rationals"
  fromInteger n = ExtRat (fromInteger n) 0

instance Eq ExtRat where
  (ExtRat a1 b1) == (ExtRat a2 b2) = (a1 == a2) && (b1 == b2)

instance Show ExtRat where
  showsPrec d (ExtRat a b) r = "(" ++ (showsPrec d a "") ++ ", " ++ (showsPrec d b "") ++ ")" ++ r


{-
  Extended complex numbers.
-}
data ExtCpx = ExtCpx ExtRat ExtRat

instance Num ExtCpx where
  (ExtCpx a1 b1) + (ExtCpx a2 b2) = ExtCpx (a1 + a2) (b1 + b2)
  (ExtCpx a1 b1) * (ExtCpx a2 b2) = ExtCpx (a1 * a2 - b1 * b2) (a1 * b2 + a2 * b1)
  negate (ExtCpx a b) = ExtCpx (negate a) (negate b)
  abs (ExtCpx a b) = ExtCpx (a * a + b * b) 0
  signum (ExtCpx a b) = error "signum undefined for extended complex numbers"
  fromInteger n = ExtCpx (fromInteger n) 0

instance Eq ExtCpx where
  (ExtCpx a1 b1) == (ExtCpx a2 b2) = (a1 == a2) && (b1 == b2)

instance Show ExtCpx where
  showsPrec d (ExtCpx a b) r = (showsPrec d a "") ++ " + i*" ++ (showsPrec d b "") ++ r

{-
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
  ./MUB-Table-exact outfile
-}
main = do
  argH : argT <- getArgs
  writeFile argH (csv_table vec_table)
  --putStr ("Have " ++ (show $ length all_vecs) ++ " vectors.\n\n")
  --putStr ("And here they are: " ++ (show all_vecs) ++ "\n")
-}

main = do putStr("Hello, World\n")

