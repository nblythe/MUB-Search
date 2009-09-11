{-
  A type defining the cyclotomic field Q(Zeta_24).
  This type implements the Num class and so can be used for typical mathematics.
  It is exact; no rounding is used.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

module Cyclotomic24 (Cyclotomic24 (Cyclotomic24), realRational, fromCyclotomic24, compRationalRealCyclotomic24, absRealCyclotomic24) where

import Complex

{-
  Type definition.
-}
data Cyclotomic24 = Cyclotomic24 Rational Rational Rational Rational Rational Rational Rational Rational


{-
  An element in Q(Zeta_24) is of the Num class.
-}
instance Num Cyclotomic24 where


{-
  Addition.
-}
  (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) + (Cyclotomic24 y0 y1 y2 y3 y4 y5 y6 y7)  = Cyclotomic24 (x0 + y0)
                                                                                                  (x1 + y1)
                                                                                                  (x2 + y2)
                                                                                                  (x3 + y3)
                                                                                                  (x4 + y4)
                                                                                                  (x5 + y5)
                                                                                                  (x6 + y6)
                                                                                                  (x7 + y7)


{-
  Multiplication.
-}
  (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) * (Cyclotomic24 y0 y1 y2 y3 y4 y5 y6 y7)  = Cyclotomic24 ((-6 * x7 * y7) + (-3 * x6 * y6) + (-2 * x5 * y5) + (-1 * x4 * y4) + (6 * x3 * y3) + (3 * x2 * y2) + (2 * x1 * y1) + (1 * x0 * y0))
                                                                                                  ((-3 * x6 * y7) + (-3 * x7 * y6) + (-1 * x4 * y5) + (-1 * x5 * y4) + (3 * x2 * y3) + (3 * x3 * y2) + (1 * x0 * y1) + (1 * x1 * y0))
                                                                                                  ((-2 * x5 * y7) + (-1 * x4 * y6) + (-2 * x7 * y5) + (-1 * x6 * y4) + (2 * x1 * y3) + (1 * x0 * y2) + (2 * x3 * y1) + (1 * x2 * y0))
                                                                                                  ((-1 * x4 * y7) + (-1 * x5 * y6) + (-1 * x6 * y5) + (-1 * x7 * y4) + (1 * x0 * y3) + (1 * x1 * y2) + (1 * x2 * y1) + (1 * x3 * y0))
                                                                                                  (( 6 * x3 * y7) + ( 3 * x2 * y6) + ( 2 * x1 * y5) + ( 1 * x0 * y4) + (6 * x7 * y3) + (3 * x6 * y2) + (2 * x5 * y1) + (1 * x4 * y0))
                                                                                                  (( 3 * x2 * y7) + ( 3 * x3 * y6) + ( 1 * x0 * y5) + ( 1 * x1 * y4) + (3 * x6 * y3) + (3 * x7 * y2) + (1 * x4 * y1) + (1 * x5 * y0))
                                                                                                  (( 2 * x1 * y7) + ( 1 * x0 * y6) + ( 2 * x3 * y5) + ( 1 * x2 * y4) + (2 * x5 * y3) + (1 * x4 * y2) + (2 * x7 * y1) + (1 * x6 * y0))
                                                                                                  (( 1 * x0 * y7) + ( 1 * x1 * y6) + ( 1 * x2 * y5) + ( 1 * x3 * y4) + (1 * x4 * y3) + (1 * x5 * y2) + (1 * x6 * y1) + (1 * x7 * y0))


{-
  Additive inverse.
-}
  negate (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7)                                    = Cyclotomic24 (negate x0)
                                                                                                  (negate x1)
                                                                                                  (negate x2)
                                                                                                  (negate x3)
                                                                                                  (negate x4)
                                                                                                  (negate x5)
                                                                                                  (negate x6)
                                                                                                  (negate x7)


{-
  Absolute value.  Computes square of the Euclidean distance from 0.
-}
  abs (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7)                                       =       ((y * y) + (z * z))
                                                                                     where y = Cyclotomic24 x0 x1 x2 x3 0 0 0 0
                                                                                           z = Cyclotomic24 x4 x5 x6 x7 0 0 0 0


{-
  Sign is left undefined and throws an error if used.
-}
  signum (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7)                                    = error "signum undefined"


{-
  Conversion from integer to element in Q(Zeta_24).
-}
  fromInteger n                                                                    = Cyclotomic24 (fromInteger n) 0 0 0 0 0 0 0


{-
  An element in Q(Zeta_24) is of the Eq class.
-}
instance Eq Cyclotomic24 where


{-
  Equality comparison.
-}
  (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) == (Cyclotomic24 y0 y1 y2 y3 y4 y5 y6 y7) =    (x0 == y0)
                                                                                     && (x1 == y1)
                                                                                     && (x2 == y2)
                                                                                     && (x3 == y3)
                                                                                     && (x4 == y4)
                                                                                     && (x5 == y5)
                                                                                     && (x6 == y6)
                                                                                     && (x7 == y7)


{-
  An element in Q(Zeta_24) is of the Show class.
-}
instance Show Cyclotomic24 where


{-
  Conversion from an element in Q(Zeta_24) to a string.
-}
  showsPrec v (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) r                             = "(" ++ (showsPrec v x0 "") ++ ", "
                                                                                         ++ (showsPrec v x1 "") ++ ", "
                                                                                         ++ (showsPrec v x2 "") ++ ", "
                                                                                         ++ (showsPrec v x3 "") ++ ", "
                                                                                         ++ (showsPrec v x4 "") ++ ", "
                                                                                         ++ (showsPrec v x5 "") ++ ", "
                                                                                         ++ (showsPrec v x6 "") ++ ", "
                                                                                         ++ (showsPrec v x7 "") ++ ")" ++ r





{-
  Extract the real rational part of a cyclotomic tuple.
-}
realRational (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) = x0

{-
  Convert a cyclotomic tuple to a complex number.
-}
fromCyclotomic24 (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) = (  (fromRational x0)
                                                           + ((fromRational x1) * sqrt(2))
                                                           + ((fromRational x2) * sqrt(3))
                                                           + ((fromRational x3) * sqrt(6)) )
                                                          :+
                                                          (  (fromRational x4)
                                                           + ((fromRational x5) * sqrt(2))
                                                           + ((fromRational x6) * sqrt(3))
                                                           + ((fromRational x7) * sqrt(6)) )



{-
  Find the kth iteration of converging upper and lower bounds on the sqrt(x).
-}
boundSqrt :: Rational -> Integer -> (Rational, Rational)
boundSqrt x 0 = (head s, head g)
                where s = Prelude.filter ((< x) . (^2)) [x, (x-1)..]
                      g = Prelude.filter ((> x) . (^2)) [1..]
boundSqrt x k = if   (c^2) < x
                then (c, u)
                else (l, c)
                where p = boundSqrt x (k - 1)
                      l = fst p
                      u = snd p
                      c = (l + u) / 2


{-
  Find the kth iteration of converging upper and lower bounds on the real part of
  a cyclotomic tuple.
-}
boundRealCyclotomic24 (Cyclotomic24 x0 x1 x2 x3 x4 x5 x6 x7) k = ( x0 + x1 * l2 + x2 * l3 + x3 * l6, x0 + x1 * u2 + x2 * u3 + x3 * u6)
                                                                 where (l2, u2) = boundSqrt 2 k
                                                                       (l3, u3) = boundSqrt 3 k
                                                                       (l6, u6) = boundSqrt 6 k


{-
  Determine if a rational is greater than or less than the real part of a cyclotomic tuple.
-}
compRationalRealCyclotomic24 :: Rational -> Cyclotomic24 -> Integer
compRationalRealCyclotomic24 x z = if (Cyclotomic24 x 0 0 0 0 0 0 0) == z
                                   then 0
                                   else if   x < bL
                                        then -1
                                        else 1
                                        where allBounds         = [boundRealCyclotomic24 z k | k <- [0..]]
                                              excludes y (l, h) = (y < l) || (y > h)
                                              (bL, bH)          = head $ Prelude.filter (excludes x) allBounds


{-
  Compute the absolute value of the real part of a cyclotomic tuple.
-}
absRealCyclotomic24 :: Cyclotomic24 -> Cyclotomic24
absRealCyclotomic24 x = if   c == 0
                        then x
                        else if   c == -1
                             then x
                             else (-x)
                        where c = compRationalRealCyclotomic24 0 x

