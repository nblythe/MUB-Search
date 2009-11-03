{-
  A representation of cyclotomic field elements
  2009 Nathan Blythe, Dr. Oscar Boykin

  This module provides a representation of elements of a cyclotomic field - a field extension of the
  rationals that adjoins a primitive nth root of unity.  In this implementation, only cyclotomic
  fields with n = 2^m (where m is any natural number) are supported.
-}

module Cyclotomic where -- (Cyclotome, approx, boundMag2, cycloGamma, cycloZero, cycloOne, rootsOfUnity) where

import Ratio
import Complex(Complex)


{-
  Type definition for members of a cyclotomic field.
-}
data Cyclotome = CycloRat Rational
                  | CycloVal Cyclotome Cyclotome Cyclotome


{-
  Cyclotomes instantiate the 'Num' class.
-}
instance Num Cyclotome where
  (CycloRat x0) + (CycloRat x1) = CycloRat (x0 + x1)
  (CycloRat x0) + (CycloVal x1 gamma1 y1) = error "Field mismatch"
  (CycloVal x0 gamma0 y0) + (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 + x1) gamma0 (y0 + y1)
                                                      else error "Gamma mismatch"

  (CycloRat x0) - (CycloRat x1) = CycloRat (x0 - x1)
  (CycloRat x0) - (CycloVal x1 gamma1 y1) = error "Field mismatch"
  (CycloVal x1 gamma1 y1) - (CycloRat x0) = error "Field mismatch"
  (CycloVal x0 gamma0 y0) - (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 - x1) gamma0 (y0 - y1)
                                                      else error "Gamma mismatch"

  (CycloRat g) * (CycloVal x gamma y) = CycloVal ((CycloRat g) * x) gamma ((CycloRat g) * y)
  (CycloRat x0) * (CycloRat x1) = CycloRat (x0 * x1)
  (CycloVal x0 gamma0 y0) * (CycloVal x1 gamma1 y1) = if   gamma0 == gamma1
                                                      then CycloVal (x0 * x1 + y0 * y1 * gamma0) gamma0 (x0 * y1 + x1 * y0)
                                                      else error "Gamma mismatch"

  fromInteger n = CycloRat (fromInteger n)

  abs x = undefined
  signum x = undefined


{-
  Cyclotomes instantiate the 'Eq' class, inherited through the 'Num' class.
-}
instance Eq Cyclotome where
  (CycloRat x0) == (CycloRat x1) = x0 == x1
  (CycloVal x0 gamma0 y0) == (CycloVal x1 gamma1 y1) = (x0 == x1) && (gamma0 == gamma1) && (y0 == y1)


{-
  Cyclotomes instantiate the 'Show' class, inherited through the 'Num' class.
-}
instance Show Cyclotome where
  showsPrec v (CycloRat x) r = (showsPrec v (numerator x) "") ++ " / " ++ (showsPrec v (denominator x) "") ++ r
  showsPrec v (CycloVal x gamma y) r = "(" ++ (showsPrec v x "") ++ ", " ++ (showsPrec v gamma "") ++ ", " ++ (showsPrec v y "") ++ ")" ++ r


{-
  Conjugate.
-}
--conj (CycloRat x) = CycloRat x
--conj (CycloVal (CycloRat x) (CycloRat gamma) (CycloRat y)) = CycloVal (CycloRat x) (CycloRat gamma) (CycloRat (-y))
--conj (CycloVal x gamma y) = CycloVal (conj x) gamma (conj y)


{-
  Multiplicative inverse.
-}
--inv (CycloRat x) = CycloRat (1 / x)
--inv (CycloVal x gamma y) = CycloVal (x * (inv (x*x - y*y*gamma))) gamma ((-y) * (inv (x*x - y*y*gamma)))


{-
  Magnitude squared.
-}
--mag2 (CycloRat x) = CycloRat ((abs x)^2)
--mag2 (CycloVal x gamma y) = CycloVal ((mag2 x) + (mag2 y) + (y * x)) gamma (x * y * (inv gamma))


{-
  Complex number approximation.
-}
approx (CycloRat x) = fromRational x
approx (CycloVal x gamma y) = (approx x) + (sqrt $ approx gamma) * (approx y)


{-
  kth iteration of converging upper and lower rational bounds on the magnitude squared.
-}
boundMag2 k c = ((fst r)^2 + (fst i)^2, (snd r)^2 + (snd i)^2)
                where r = boundReal k c
                      i = boundImag k c


{-
  kth iteration of converging lower and upper rational bounds on the real part of the square root of gamma.
-}
boundRealOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = (0, 0)
boundRealOmega k (CycloVal x gamma y) = (l, u)
                                        where l = fst $ boundSqrt k $ (1 + (fst $ boundReal k gamma)) / 2
                                              u = snd $ boundSqrt k $ (1 + (snd $ boundReal k gamma)) / 2


{-
  kth iteration of converging lower and upper rational bounds on the imaginary part of the square root of gamma.
-}
boundImagOmega k (CycloVal (CycloRat x) (CycloRat (-1)) (CycloRat y)) = (1, 1)
boundImagOmega k (CycloVal x gamma y) = (l, u)
                                        where l = (fst $ boundImag k gamma) / (snd $ boundSqrt k (2 + 2 * (snd $ boundReal k gamma)))
                                              u = (snd $ boundImag k gamma) / (fst $ boundSqrt k (2 + 2 * (fst $ boundReal k gamma)))


{-
  kth iteration of converging lower and upper bounds on the real part.
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
  kth iteration of converging lower and upper bounds on the imaginary part.
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
  Given a list of tuples, construct a particular list of elements, one from each tuple.
-}
allArgChoices :: [(Rational, Rational)] -> Integer -> [Rational]
allArgChoices (h : t) n = if  null t
                          then [thisChoice]
                          else thisChoice : restChoices
                          where thisChoice = if   mod n 2 == 0
                                             then fst h
                                             else snd h
                                restChoices = allArgChoices t (div n 2)


{-
  kth iteration of converging upper and lower bounds on the square root of a rational number.
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

