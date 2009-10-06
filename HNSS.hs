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


buildCol :: Polynomial -> Monomial -> [(Int, Float)]
buildCol (Polynomial c p) m' = (lookup m', c) : Data.List.map (\ (k, m) -> (lookup (monomialMultiply m m'), k)) p
                               where lookup x = monomial2Int 6 x --fromJust $ findIndex (== x) hnssR

buildTbl = concat $ Data.List.map (\ m -> Data.List.map (\ p -> buildCol p m) mubPolys) hnssC


groupRow' (Polynomial c []) m' = [(m', c)]
groupRow' (Polynomial c ((k, m) : mT)) m' = (monomialMatch m' m, k) : groupRow' (Polynomial c mT) m'

buildRow :: [(Monomial, Float)] -> [(Int, Float)]
buildRow [] = []
buildRow r  = if   i == Nothing
              then rest
              else  (fromJust i, snd $ head r) : rest
              where i    = findIndex (== (fst $ head r)) hnssC
                    rest = buildRow (tail r)


groupRow p m = buildRow $ groupRow' p m


--hnss = concat [[[polynomialCoef p (monomialMatch q m) | q <- hnssR] | m <- polynomialMonomials p] | p <- mubPolys]
--hnss = concat $ Data.List.map hnss' mubPolys
--       where hnss'' p m = Data.List.map (\ q -> polynomialCoef p (monomialMatch q m)) $ monomialGen mubVariables 4
--             hnss' p = Data.List.map (hnss'' p) $ polynomialMonomials p

hnss = [[[polynomialCoef p (monomialMatch q m) | q <- hnssR] | m <- hnssC] | p <- mubPolys]


--main = do
--  print $ filter (/= 0) $ concat hnss

