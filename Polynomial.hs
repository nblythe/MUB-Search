{-
  Polynomials
  2009 Nathan Blythe, Dr. Oscar Boykin

  Functions and types for working with polynomials based on sets of MUBs.
-}

module Polynomial (RoI(Real, Imag), Variable(Variable), Sonomial(Sonomial), Monomial(Monomial), Polynomial(Polynomial),
                   sonomialMultiply, sonomialInverse, allSonomials,
                   monomialDegree, monomialMultiply, monomialInverse, monomialMatch, allMonomials, monomial2Int, int2Monomial, numMonomials,
                   polynomialAdd, polynomialMultiply, polynomialMonomials, polynomialCoef, polynomialStrip) where


import Data.List


{-
  Some parameters that describe the MUB problem and determine the number of variables.

  d: dimension in which we're working.
  n: number of bases, not including the standard basis.
-}
d = 6;
n = 3;




{-
  A handy factorial function.
-}
facs = scanl (*) 1 [1..]
fac :: Int -> Integer
fac n = facs !! n


{-
  A handy n-choose-k function.
-}
nchoosek :: Int -> Int -> Integer
nchoosek n k = div (fac n) ((fac (n - k)) * (fac k))




{-
  Enumeration with elements Real and Imag.
-}
data RoI = Real | Imag deriving (Eq, Show)


{-
  RoI instantiates Ord for use in the Variable type's Ord instantiation.
-}
instance Ord RoI where
  compare Real Real = EQ
  compare Real Imag = LT
  compare Imag Real = GT
  compare Imag Imag = EQ




{-
  Data type describing variables.
-}
data Variable =   Variable RoI Int Int Int deriving (Eq)


{-
  Variable instantiates Ord in order to instantiate Enum.
-}
instance Ord Variable where
  compare (Variable roi0 i0 j0 k0) (Variable roi1 i1 j1 k1) | k0 > k1 = GT
                                                            | k0 < k1 = LT
                                                            | j0 > j1 = GT
                                                            | j0 < j1 = LT
                                                            | i0 > i1 = GT
                                                            | i0 < i1 = LT
                                                            | roi0 > roi1 = GT
                                                            | roi0 < roi1 = LT
                                                            | otherwise = EQ


{-
  Variable instantiates Enum for easy generation of Variables.
-}
instance Enum Variable where
  fromEnum (Variable roi i j k) = nroi + i * 2 + j * 2 * d + k * 2 * d^2
                                  where nroi | roi == Real = 0
                                             | roi == Imag = 1

  toEnum x = Variable roi i j k
             where roi | mod x 2 == 0 = Real
                       | mod x 2 == 1 = Imag
                   i = mod (div x 2) d
                   j = mod (div x (2 * d)) d
                   k = div x (2 * d^2)


{-
  Variable instantiates Show for pretty-printing.
-}
instance Show Variable where
  showsPrec v x r = "(x" ++ (show $ fromEnum x) ++ ")" ++ r


{-
  All variables.
-}
allVariables = [Variable Real 0 0 0 .. Variable Imag (d - 1) (d - 1) (n - 1)]
numVariables = 2 * d^2 * n



{-
  A Sonomial is a Variable raised to an integral exponent.
-}
data Sonomial = Sonomial Variable Int deriving (Eq)


{-
  Sonomial instantiates Enum for easy generation.
-}
instance Enum Sonomial where
  fromEnum (Sonomial v n) = (n - 1) * numVariables + (fromEnum v)

  toEnum x = Sonomial v n
             where n = 1 + (div x numVariables)
                   v = toEnum $ mod x numVariables


{-
  Sonomial instantiates Show for pretty-printing.
-}
instance Show Sonomial where
  showsPrec v (Sonomial x n) r = (show x) ++ "^" ++ (show n) ++ r


{-
  Multiply two Sonomials.  Undefined if the sonomials are not of the same variable.
-}
sonomialMultiply (Sonomial s0 n0) (Sonomial s1 n1) | s0 == s1  = (Sonomial s0 (n0 + n1))
                                                   | otherwise = undefined


{-
  Multiplicative inverse of a Sonomial.
-}
sonomialInverse  (Sonomial s n) = Sonomial s (-n)


{-
  All sonomials with degree less than or equal to d.
-}
allSonomials :: Int -> [Sonomial]
allSonomials d = [Sonomial (head allVariables) 1 .. Sonomial (last allVariables) d]




{-
  A Monomial is a product of Sonomials.
-}
data Monomial = Monomial [Sonomial]


{-
  Monomial instantiates Show for pretty-printing.
-}
instance Show Monomial where
  showsPrec v (Monomial (xH : xT)) r = if   null xT
                                       then (show xH) ++ r
                                       else (show xH) ++ " " ++ (showsPrec v (Monomial xT) r)


{-
  Monomial instantiates Eq to provide equality under permutations.
-}
instance Eq Monomial where
  (Monomial xL) == (Monomial yL) =    (all (\ x -> any (== x) yL) xL)
                                   && (all (\ y -> any (== y) xL) yL)


{-
  Degree of a Monomial.
-}
monomialDegree (Monomial []) = 0
monomialDegree (Monomial ((Sonomial v n) : sT)) = n + (monomialDegree (Monomial sT))


{-
  Number of monomials with a degree less than or equal to d.
-}
numMonomials :: Int -> Integer
numMonomials d = sum $ map (\ k -> nchoosek (numVariables + k - 1) k) [1 .. d]


{-
  Expand a Monomial to a sorted list of Variables.
-}
monomialExpand' :: Monomial -> [Variable]
monomialExpand' (Monomial ((Sonomial v n) : sT)) = if   null sT
                                                   then replicate n v
                                                   else (replicate n v) ++ (monomialExpand' (Monomial sT))

monomialExpand :: Monomial -> [Variable]
monomialExpand m = Data.List.sort $ monomialExpand' m


{-
  Contract a sorted list of Variables to a Monomial.
-}
monomialContract :: [Variable] -> Monomial
monomialContract (vH : vT) = if   null vT
                             then Monomial [Sonomial vH 1]
                             else monomialMultiply' (monomialContract vT) (Sonomial vH 1)


{-
  Compute the unique integer that corresponds to a sorted list of Variables.
-}
monomial2Int' :: [Variable] -> Integer
monomial2Int' (lH : [])        = 0
monomial2Int' (lH1 : lH2 : lT) = (f lH1) - (f lH2) + monomial2Int' (lH2 : lT)
                                 where f v = nchoosek (numVariables - (fromEnum v) + (length lT)) (1 + length lT)


{-
  Compute the unique integer that corresponds to a Monomial.
-}
monomial2Int :: Monomial -> Integer
monomial2Int m = numMonomials ((length m') - 1) + (monomial2Int' ((toEnum 0) : m'))
                 where m' = monomialExpand m


{-
  Compute the degree of a unique Monomial corresponding to an Integer.
-}
int2Monomial'' :: Integer -> Int
int2Monomial'' x = fst $ head $ filter (\ (d, i) -> i > x) dIndices
                   where dIndices = map (\ d -> (d, numMonomials d)) [1 ..]


{-
  Compute the unique ordered list of Variables corresponding to the ith Monomial of
  degree d whose Variables have corresponding integers greater than or equal to p.
-}
int2Monomial' :: Int -> Int -> Integer -> [Variable]
int2Monomial' _ 1 i = [toEnum (fromInteger i)]
int2Monomial' p d i = (toEnum j) : int2Monomial' j (d - 1) (i' j)
                      where i' v = i - ((nchoosek (numVariables + d - 1) d) - (nchoosek (numVariables - v + d - 1) d))
                                     + ((nchoosek (numVariables + d - 2) (d - 1)) - (nchoosek (numVariables - v + d - 2) (d - 1)))
                            j = head $ filter (\ v -> ((i' v) >= 0) && (i' v) < (numMonomials (d - 1)) && (v >= p)) [0 .. numVariables - 1]
                            nNextMonomials v = nchoosek (numVariables - v + d - 2) (d - 1)


{-
  Compute the unique Monomial corresponding to an Integer.
-}
int2Monomial :: Integer -> Monomial
int2Monomial x = monomialContract l
                 where d = int2Monomial'' x
                       l = int2Monomial' 0 d $ x - (numMonomials (d - 1))


{-
  Generate all monomials of degree d or less.
-}

allMonomials'' :: Int -> Int -> [Monomial]
allMonomials'' v 0 = [Monomial [Sonomial (allVariables !! v) 1]]
allMonomials'' v d = [ monomialMultiply' m (Sonomial (allVariables !! v) 1) | m <- nm ]
                     where nm = concat [allMonomials'' w (d - 1) | w <- [v .. numVariables - 1]]

allMonomials' :: Int -> [Monomial]
allMonomials' d = concat $ map (\ v -> allMonomials'' v (d - 1)) [0 .. numVariables - 1]

allMonomials :: Int -> [Monomial]
allMonomials d = concat $ map allMonomials' [1 .. d]


--allMonomials :: Int -> [Monomial]
--allMonomials d = map int2Monomial [0 .. numMonomials d]


{-
  Multiply a Monomial by a Sonomial.
-}
monomialMultiply' :: Monomial -> Sonomial -> Monomial
monomialMultiply' (Monomial [])        s = Monomial [s]
monomialMultiply' (Monomial ((Sonomial v n) : mT))
                  (Sonomial v' n') = if   v == v'
                                     then monomialMultiply' (Monomial mT) (sonomialMultiply (Sonomial v n) (Sonomial v' n'))
                                     else Monomial ((Sonomial v n) : mL)
                                     where (Monomial mL) = monomialMultiply' (Monomial mT) (Sonomial v' n')


{-
  Multiply two Monomials.
-}
monomialMultiply :: Monomial -> Monomial -> Monomial
monomialMultiply m0 (Monomial (m1H : m1T)) = if   null m1T
                                             then monomialMultiply' m0 m1H
                                             else monomialMultiply (monomialMultiply' m0 m1H) (Monomial m1T)


{-
  Multiplicative inverse of a Monomial.
-}
monomialInverse :: Monomial -> Monomial
monomialInverse (Monomial m) = Monomial $ map sonomialInverse m


{-
  Given a monomial m and a sonomial s, find a sonomial t such that m contains (s t).
-}
monomialMatch' :: Monomial -> Sonomial -> Sonomial
monomialMatch' (Monomial m) (Sonomial v n) = if   null r
                                             then sonomialInverse (Sonomial v n)
                                             else Sonomial v (n' - n)
                                             where r = filter (\ (Sonomial w l) -> w == v) m
                                                   Sonomial v' n' = head r


{-
  Given monomials x and y, find a monomial m such that x = (m y).
-}
monomialMatch :: Monomial -> Monomial -> Monomial
monomialMatch (Monomial x) (Monomial y) = Monomial [monomialMatch' (Monomial x) s | s <- y]


{-
  A Polynomial is a constant and a list of Monomials with coefficients.
-}
data Polynomial = Polynomial Float [(Float, Monomial)]


{-
  Polynomial instantiates Show for pretty-printing.
-}
instance Show Polynomial where
  showsPrec v (Polynomial c ((k, m) : mT)) r = if   null mT
                                               then (show k) ++ " " ++ (show m) ++ " + " ++ (show c) ++ r
                                               else (show k) ++ " " ++ (show m) ++ " + " ++ (showsPrec v (Polynomial c mT) r)


{-
  Polynomial instantiates Eq to provide equality under permutations.
-}
instance Eq Polynomial where
  (Polynomial c0 p0) == (Polynomial c1 p1) = if   c0 == c1
                                             then    (all (\ x -> (any (== x) p1)) p0)
                                                  && (all (\ y -> (any (== y) p0)) p1)
                                             else False


{-
  Add a Monomial to a Polynomial.
-}
polynomialAdd' :: Polynomial -> Float -> Monomial -> Polynomial
polynomialAdd' (Polynomial c []) a m = Polynomial c [(a, m)]
polynomialAdd' (Polynomial c ((a', m') : mT)) a m = if   m' == m
                                                    then if   a' + a == 0
                                                         then Polynomial c mT
                                                         else Polynomial c ((a' + a, m') : mT)
                                                    else if   a' == 0
                                                         then Polynomial c mT'
                                                         else Polynomial c ((a', m') : mT')
                                                    where Polynomial _ mT' = polynomialAdd' (Polynomial c mT) a m


{-
  Add two Polynomials.
-}
polynomialAdd :: Polynomial -> Polynomial -> Polynomial
polynomialAdd (Polynomial c0 m0) (Polynomial c1 [])             = Polynomial (c0 + c1) m0
polynomialAdd (Polynomial c0 m0) (Polynomial c1 ((a, m) : m1T)) = if   null m1T
                                                                  then polynomialAdd' (Polynomial (c0 + c1) m0) a m
                                                                  else polynomialAdd (polynomialAdd' (Polynomial c0 m0) a m) (Polynomial c1 m1T)


{-
  Multiply a Polynomial by a constant.
-}
polynomialMultiply''' :: Polynomial -> Float -> Polynomial
polynomialMultiply''' (Polynomial c p) c' = Polynomial (c * c') $ map (\ (k, m) -> (k * c', m)) p


{-
  Multiply two Monomials with coefficients.
-}
polynomialMultiply'' :: (Float, Monomial) -> (Float, Monomial) -> (Float, Monomial)
polynomialMultiply'' (k0, m0) (k1, m1) = (k0 * k1, monomialMultiply m0 m1)


{-
  Multiply a Polynomial by a Monomial with coefficient.
-}
polynomialMultiply' :: Polynomial -> (Float, Monomial) -> Polynomial
polynomialMultiply' (Polynomial c p) (k, m) = if   c * k == 0
                                              then Polynomial 0 r
                                              else Polynomial 0 $ (c * k, m) : r
                                              where r = [polynomialMultiply'' (k, m) (k', m') | (k', m') <- p]


{-
  Multiply two polynomials.
-}
polynomialMultiply :: Polynomial -> Polynomial -> Polynomial
polynomialMultiply x (Polynomial c p) = if   null p
                                        then polynomialMultiply''' x c
                                        else polynomialAdd (polynomialMultiply' x (head p)) (polynomialMultiply x (Polynomial c (tail p)))


{-
  Get all the monomials from a polynomial.
-}
polynomialMonomials :: Polynomial -> [Monomial]
polynomialMonomials (Polynomial c p) = snd $ unzip p


{-
  Get the coefficient accompanying a particular monomial from a polynomial.
-}
polynomialCoef :: Polynomial -> Monomial -> Float
polynomialCoef (Polynomial c p) m' = if   null r
                                     then 0
                                     else fst (head r)
                                     where r = filter (\ (k, m) -> m == m') p


{-
  Remove terms from a polynomial that have coefficient zero.

  In general polynomials shouldn't have lots of zeros hanging around as they're removed in
  some functions as they occur, but it happens.
-}
polynomialStrip :: Polynomial -> Polynomial
polynomialStrip (Polynomial c p) = Polynomial c $ filter (\ (k, m) -> k /= 0) p

