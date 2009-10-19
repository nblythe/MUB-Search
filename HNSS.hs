{-
  Generate the linear system for Hilbert's Nullstellensatz for the MUB problem in dimension 6
  with quadratic multiplying polynomials.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

import System(getArgs)

import Polynomial


{-
  A polynomial of type 1.
-}
mubPoly1 i j k = Polynomial (-1) [(1, Monomial [Sonomial (Variable Real i j k) 2]),
                                  (1, Monomial [Sonomial (Variable Imag i j k) 2])] 


{-
  A polynomial of type 2.
-}
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
                            then polynomialAdd b (Polynomial (-36) [])
                            else polynomialAdd b (Polynomial 0 [])
                       else polynomialAdd b (Polynomial (-6) [])
                       where x = mubPoly2a j1 j2 k1 k2
                             x2 = polynomialMultiply x x
                             y = mubPoly2b j1 j2 k1 k2
                             y2 = polynomialMultiply y y
                             b = polynomialAdd x2 y2


{-
  All polynomials.
-}
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


{-
  Compute a cell in the linear system table.
-}
tblCell :: Int -> Int -> Int -> (Integer, Int, Float)
tblCell p m c | c == 0 && m == 0 = (-1, p * nColumns, k)                            -- constant term in polynomial, constant column.
              | c == 0 && m /= 0 = (monomial2Int monomial, p * nColumns, coef)      -- non-constant term in polynomial, constant column.
              | c /= 0 && m == 0 = (monomial2Int monomial', p * nColumns + c, k)    -- constant term in polynomial, non-constant column.
              | c /= 0 && m /= 0 = (r, p * nColumns + c, coef)                      -- non-constant term in polynomial, non-constant column.
                where polynomial = mubPolys !! p
                      (Polynomial k _) = polynomial
                      monomial = polynomialMonomials polynomial !! (m - 1)
                      coef = polynomialCoef polynomial monomial
                      monomial' = hnssC !! (c - 1)
                      r = monomial2Int $ monomialMultiply monomial monomial'


{-
  Compute the contents of the linear system table for polynomials p1 through p2.
-}
tbl :: Int -> Int -> [(Integer, Int, Float)]
tbl p1 p2 = concat $ map tblGrp [p1 .. p2]
      where tblGrpCol p c = map (\ m -> tblCell p m c) [0 .. (nMonomials !! p) - 1]
            tblGrp p      = concat $ map (tblGrpCol p) [0 .. nColumns - 1]


{-
  Prettified output for polynomials p1 through p2.
-}
tblPretty :: Int -> Int -> [String]
tblPretty p1 p2 = map (\ (r, c, f) -> (show r) ++ "," ++ (show c) ++ "," ++ (show f)) $ tbl p1 p2


{-
  HNSS s j m

  s: offset for job index.
  j: job index.
  m: job size.

  Writes all linear system entries for polynomials (s + j) * m through (s + j + 1) * m - 1
  to the standard output.
-}
main = do
  {-
    Command line arguments.
  -}
  sS : (sJ : (sM : argsT)) <- getArgs
  let s = read sS :: Int
  let j = read sJ :: Int
  let m = read sM :: Int


  {-
    Starting and ending polynomials for this job.
  -}
  let p1 = (s + j) * m
  let p2 = (s + j + 1) * m - 1


  {-
    Write to stdout.
  -}
  mapM_ putStrLn $ if   p2 < nGroups
                   then (tblPretty p1 p2)
                   else []


