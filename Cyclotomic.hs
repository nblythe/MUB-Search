{-
  An exact representation of cyclotomic fields
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  This module provides an exact representation of elements in a cyclotomic
  field - an extension field of the rationals that adjoins a primitive nth root
  of unity.  In this implementation, only cyclotomic fields with n = 2^m (where
  m is any natural number) are supported.
-}

module Cyclotomic (Cyclotome, approx, boundMag2, cycloGamma, cycloZero, cycloOne, rootsOfUnity) where

import Ratio
import Complex(Complex)


{-
  Type definition for members of a cyclotomic field.

  The structure is a full ternary tree.  Leafs hold a rational value. Nodes
  hold no value; they are defined entirely by their children.

  A node has three children, which are read left to right as x, gamma, and y.
  A node represents the value x + sqrt(gamma) * y, where sqrt is the principle
  square root function.

  The height of a tree and the fact that it is full and ternary determines its
  shape.  The gammas attached to each node collectively determine its "color".
  Two trees with identical shape and color describe cyclotomes from the same
  cyclotomic field.

  Mathematically this is a sufficient, but not necessary, condition, but for
  the purposes of implementation it is required.  If two trees have different
  shape a "field mismatch" occurs.  If two trees have different color a "gamma
  mismatch" occurs.  Sometimes it's not possible to determine which mismatch
  really occurred.
-}
data Cyclotome = CycloRat Rational
                  | CycloVal Cyclotome Cyclotome Cyclotome



{-
  Cyclotomes instantiate the 'Num' class.
-}
instance Num Cyclotome where
  {-
    Addition is pointwise, with gammas left alone.  The trees must be the same
    height (and since they are necessarily full, the same shape).  If they are
    not the same height a "field mismatch" or a "color mismatch" occurs.

    The trees must have the same color.  If they are not the same color a
    "color mismatch" occurs.
  -}
  (CycloRat x0) + (CycloRat x1) = CycloRat (x0 + x1)
  (CycloRat x0) + (CycloVal x1 gamma1 y1) = error "Field mismatch"
  (CycloVal x0 gamma0 y0) + (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 + x1) gamma0 (y0 + y1)
                                                      else error "Color mismatch"


  {-
    Subtraction follows addition.
  -}
  (CycloRat x0) - (CycloRat x1) = CycloRat (x0 - x1)
  (CycloRat x0) - (CycloVal x1 gamma1 y1) = error "Field mismatch"
  (CycloVal x1 gamma1 y1) - (CycloRat x0) = error "Field mismatch"
  (CycloVal x0 gamma0 y0) - (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 - x1) gamma0 (y0 - y1)
                                                      else error "Color mismatch"


  {-
    Multiplication is complex (but not complicated!).  The trees must be the
    same height and color.  If they are not the same height and color a "color
    mismatch" occurs.

    The exception is multiplication of a leaf against a full tree, which is
    allowed.
  -}
  (CycloRat g) * (CycloVal x gamma y) = CycloVal ((CycloRat g) * x) gamma ((CycloRat g) * y)
  (CycloRat x0) * (CycloRat x1) = CycloRat (x0 * x1)
  (CycloVal x0 gamma0 y0) * (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 * x1 + y0 * y1 * gamma0) gamma0 (x0 * y1 + x1 * y0)
                                                      else error "Color mismatch"


  {-
    Constructing a cyclotome from an integer always produces a leaf.  To
    promote rationals into cyclotomic fields, multiply against a tree
    constructed with cycloOne.
  -}
  fromInteger n = CycloRat (fromInteger n)


  {-
    The absolute value of a complex number is undefined.
  -}
  abs x = undefined


  {-
    The sign of a complex number is undefined.
  -}
  signum x = undefined



{-
  Cyclotomes instantiate the 'Eq' class, inherited through the 'Num' class.

  Two cyclotomes are equivalent if they are exactly identical.  This doesn't
  account for multiple numerically-equivalent constructions, so these routines
  aren't to be used unless you pay attention to what you're doing and verify
  that you won't accidentally call two cyclotomes unequal that are simply
  constructed differently.
-}
instance Eq Cyclotome where
  (CycloRat x0) == (CycloRat x1) = x0 == x1
  (CycloVal x0 gamma0 y0) == (CycloVal x1 gamma1 y1) =    (x0 == x1)
                                                       && (gamma0 == gamma1)
                                                       && (y0 == y1)



{-
  Cyclotomes instantiate the 'Show' class, inherited through the 'Num' class.
-}
instance Show Cyclotome where
  showsPrec v (CycloRat x) r = (showsPrec v (numerator x) "") ++ " / " ++ (showsPrec v (denominator x) "") ++ r
  showsPrec v (CycloVal x gamma y) r = "(" ++ (showsPrec v x "") ++ ", " ++ (showsPrec v gamma "") ++ ", " ++ (showsPrec v y "") ++ ")" ++ r



{-
  Approximation of a complex number as a complex floating point number.

  This is fast and easy but not to be used other than visualization.
-}
approx (CycloRat x) = fromRational x
approx (CycloVal x gamma y) = (approx x) + (sqrt $ approx gamma) * (approx y)



{-
  kth iteration of converging upper and lower rational bounds on the magnitude
  squared of a cyclotome.
-}
boundMag2 k c = ((fst r)^2 + (fst i)^2, (snd r)^2 + (snd i)^2)
                where r = boundReal k c
                      i = boundImag k c



{-
  kth iteration of converging lower and upper rational bounds on the real part
  of the square root of gamma in a cyclotome.
-}
boundRealOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = (0, 0)
boundRealOmega k (CycloVal x gamma y) = (l, u)
                                        where l = fst $ boundSqrt k $ (1 + (fst $ boundReal k gamma)) / 2
                                              u = snd $ boundSqrt k $ (1 + (snd $ boundReal k gamma)) / 2



{-
  kth iteration of converging lower and upper rational bounds on the imaginary
  part of the square root of gamma in a cyclotome.
-}
boundImagOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = (1, 1)
boundImagOmega k (CycloVal x gamma y) = (l, u)
                                        where l = (fst $ boundImag k gamma) / (snd $ boundSqrt k (2 + 2 * (snd $ boundReal k gamma)))
                                              u = (snd $ boundImag k gamma) / (fst $ boundSqrt k (2 + 2 * (fst $ boundReal k gamma)))



{-
  kth iteration of converging lower and upper bounds on the real part of
  a cyclotome.
-}
boundReal k (CycloRat x) = (x, x)
boundReal k (CycloVal x gamma y) = (min, max)
                                   where a  = boundReal k x
                                         b  = boundReal k y
                                         c  = boundRealOmega k (CycloVal x gamma y)
                                         d  = boundImag k y
                                         e  = boundImagOmega k (CycloVal x gamma y)
                                         min = (fst a) + (minimum valsBC) - (maximum valsDE)
                                         max = (snd a) + (maximum valsBC) - (minimum valsDE)
                                         valsBC = [(fst b) * (fst c), (fst b) * (snd c), (snd b) * (fst c), (snd b) * (snd c)]
                                         valsDE = [(fst d) * (fst e), (fst d) * (snd e), (snd d) * (fst e), (snd d) * (snd e)]



{-
  kth iteration of converging lower and upper bounds on the imaginary part of
  a cyclotome.
-}
boundImag k (CycloRat x) = (0, 0)
boundImag k (CycloVal x gamma y) = (min, max)
                                   where a  = boundImag k x
                                         b  = boundImag k y
                                         c  = boundRealOmega k (CycloVal x gamma y)
                                         d  = boundReal k y
                                         e  = boundImagOmega k (CycloVal x gamma y)
                                         min = (fst a) + (minimum valsBC) + (minimum valsDE)
                                         max = (snd a) + (maximum valsBC) + (maximum valsDE)
                                         valsBC = [(fst b) * (fst c), (fst b) * (snd c), (snd b) * (fst c), (snd b) * (snd c)]
                                         valsDE = [(fst d) * (fst e), (fst d) * (snd e), (snd d) * (fst e), (snd d) * (snd e)]



{-
  A primitive (2^m)th root of unity in the cyclotomic field Q(Zeta_{2^m}).
-}
cycloGamma 0 = CycloRat 1
cycloGamma 1 = CycloRat (-1)
cycloGamma m = CycloVal (cycloZero (m - 1)) (cycloGamma (m - 1)) (cycloOne  (m - 1))



{-
  The number 0 in the cyclotomic field Q(Zeta_{2^m}).
-}
cycloZero  0 = CycloRat 0
cycloZero  1 = CycloRat 0
cycloZero  m = CycloVal (cycloZero (m - 1)) (cycloGamma (m - 1)) (cycloZero (m - 1))



{-
  The number 1 in the cyclotomic field Q(Zeta_{2^m}).
-}
cycloOne   0 = CycloRat 1
cycloOne   1 = CycloRat 1
cycloOne   m = CycloVal (cycloOne  (m - 1)) (cycloGamma (m - 1)) (cycloZero (m - 1))



{-
  The (2^m)th roots of unity.
-}
rootsOfUnity m = take (2^m) $ iterate (* (cycloGamma m)) (cycloGamma m)



{-
  kth iteration of converging upper and lower bounds on the square root of a
  rational number.
-}
boundSqrt :: Integer -> Rational -> (Rational, Rational)
boundSqrt 0 x = (head s, head g)
                where s = Prelude.filter ((< x) . (^2)) [x, (x-1)..]
                      g = Prelude.filter ((> x) . (^2)) [1..]
boundSqrt k 0 = (0, 0)
boundSqrt k x = if   (c^2) < x
                then (c, u)
                else (l, c)
                where p = boundSqrt (k - 1) x
                      l = fst p
                      u = snd p
                      c = (l + u) / 2

