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
d :: Integer
d = 6
n :: Integer
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
  12th roots of unity.
-}
rZero = ExtRat 0   0 -- 0
rHalf = ExtRat 0.5 0 -- 0.5
rSqrt = ExtRat 0   1 -- sqrt(3) / 2
rOne  = ExtRat 1   0 -- 1
roots = [ ExtCpx   rOne      rZero  ,
          ExtCpx   rSqrt     rHalf  ,
          ExtCpx   rHalf     rSqrt  ,
          ExtCpx   rZero     rOne   ,
          ExtCpx (-rHalf)    rSqrt  ,
          ExtCpx (-rSqrt)    rHalf  ,
          ExtCpx (-rOne)     rZero  ,
          ExtCpx (-rSqrt)  (-rHalf) ,
          ExtCpx (-rHalf)  (-rSqrt) ,
          ExtCpx   rZero   (-rOne)  ,
          ExtCpx   rHalf   (-rSqrt) ,
          ExtCpx   rSqrt   (-rHalf) ]


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
  argH : argT <- getArgs
  writeFile argH (lbools2bits vec_table)
  --putStr ("Have " ++ (show $ length all_vecs) ++ " vectors.\n\n")
  --putStr ("And here they are: " ++ (show all_vecs) ++ "\n")


