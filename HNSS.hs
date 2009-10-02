{-
  Generate the linear system for Hilbert's Nullstellensatz for the MUB problem
  in dimension 6, with quadratic multiplying polynomials.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

import System(getArgs)

import Polynomial


{-
  The MUB problem polynomial system.
-}
mubPoly1 i j k = Polynomial (-6) [(1, Monomial [Sonomial (Variable Real i j k) 2]),
                                  (1, Monomial [Sonomial (Variable Imag i j k) 2])] 


mubPoly2a' i j1 j2 k1 k2 = Polynomial 0 [(1,  Monomial [Sonomial (Variable Real i j1 k1) 1,
                                                        Sonomial (Variable Real i j2 k2) 1]),
                                         (-1, Monomial [Sonomial (Variable Imag i j1 k1) 1,
                                                        Sonomial (Variable Imag i j2 k2) 1])]
mubPoly2a j1 j2 k1 k2 = foldl polynomialAdd (mubPoly2a' 0 j1 j2 k1 k2) [mubPoly2a' i j1 j2 k1 k2 | i <- [1 .. 5]] 

mubPoly2b' i j1 j2 k1 k2 = Polynomial 0 [(1,  Monomial [Sonomial (Variable Real i j1 k1) 1,
                                                        Sonomial (Variable Imag i j2 k2) 1]),
                                         (1,  Monomial [Sonomial (Variable Imag i j1 k1) 1,
                                                        Sonomial (Variable Real i j2 k2) 1])]
mubPoly2b j1 j2 k1 k2 = foldl polynomialAdd (mubPoly2b' 0 j1 j2 k1 k2) [mubPoly2b' i j1 j2 k1 k2 | i <- [1 .. 5]] 

mubPoly2 j1 j2 k1 k2 = if   k1 == k2
                       then if   j1 == j2
                            then polynomialAdd b (Polynomial (-1) [])
                            else polynomialAdd b (Polynomial 0 [])
                       else polynomialAdd b (Polynomial (-6) [])
                       where x = mubPoly2a j1 j2 k1 k2
                             y = mubPoly2b j1 j2 k1 k2
                             b = polynomialAdd (polynomialMultiply x x) (polynomialMultiply y y)

mubVariables = [Variable Real 0 0 0 .. Variable Imag 5 5 2]
mubPolys1 = [mubPoly1 i j k | i <- [0 .. 5], j <- [0 .. 5], k <- [0 .. 2]]
mubPolys2 = concat [[mubPoly2 j1 j2 k1 k2 | j2 <- [j1 .. 5], k2 <- [k1 .. 2]]  |  j1 <- [0 .. 5], k1 <- [0 .. 2]]
mubPolys = mubPolys1 ++ mubPolys2


{-
  The MUB problem linear system.
-}
hnssR = monomialGen mubVariables 4
hnss = concat [[[polynomialCoef p (monomialMatch q m) | q <- hnssR] | m <- polynomialMonomials p] | p <- mubPolys]

