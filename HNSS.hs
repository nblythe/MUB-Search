{-
  Generate the linear system for Hilbert's Nullstellensatz for the MUB problem
  in dimension 6, with quadratic multiplying polynomials.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

import System(getArgs)

import Data.List
import Data.Maybe

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
hnssR = allMonomials 6
hnssC = allMonomials 2
nColumns = 1 + (length hnssC)
nGroups = length mubPolys
nMonomials = [length $ polynomialMonomials p | p <- mubPolys]


tblCell :: Int -> Int -> Int -> (Integer, Int, Float)
tblCell p m c | c == 0 && m == 0 = (-1, p * nColumns, k)
              | c == 0 && m /= 0 = (monomial2Int 6 monomial, p * nColumns, coef)
              | c /= 0 && m == 0 = (monomial2Int 6 monomial', p * nColumns + c, k)
              | c /= 0 && m /= 0 = (r, p * nColumns + c, coef)
                where polynomial = mubPolys !! p
                      (Polynomial k _) = polynomial
                      monomial = polynomialMonomials polynomial !! (m - 1)
                      coef = polynomialCoef polynomial monomial
                      monomial' = hnssC !! (c - 1)
                      r = monomial2Int 6 $ monomialMultiply monomial monomial'

tbl = concat $ Data.List.map tblGrp [0 .. nGroups - 1]
      where tblGrpCol p c = Data.List.map (\ m -> tblCell p m c) [0 .. (nMonomials !! p) - 1]
            tblGrp p      = concat $ Data.List.map (tblGrpCol p) [0 .. nColumns - 1]

tblPretty = Data.List.map (\ (r, c, f) -> (show r) ++ "," ++ (show c) ++ "," ++ (show f)) tbl

main = do
  sequence $ Data.List.map putStrLn tblPretty

