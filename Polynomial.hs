{-
  Polynomials
  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Polynomial (RoI(Real, Imag), Variable(Variable), Sonomial(Sonomial), Monomial(Monomial), Polynomial(Polynomial),
                   sonomialMultiply, sonomialInverse, allSonomials,
                   monomialMultiply, monomialInverse, monomialMatch, allMonomials, monomial2Int,
                   polynomialAdd, polynomialMultiply, polynomialMonomials, polynomialCoef, polynomialStrip) where


import Data.List


{-
  Some parameters that configure the types.
-}
vectorLength = 6;
basisWidth = 6;
numBases = 4;


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
  fromEnum (Variable roi i j k) = nroi + i * 2 + j * 2 * vectorLength + k * 2 * vectorLength * basisWidth
                                  where nroi | roi == Real = 0
                                             | roi == Imag = 1

  toEnum x = Variable roi i j k
             where roi | mod x 2 == 0 = Real
                       | mod x 2 == 1 = Imag
                   i = mod (div x 2) vectorLength
                   j = mod (div x (2 * vectorLength)) basisWidth
                   k = div x (2 * vectorLength * basisWidth)


{-
  Variable instantiates Show for pretty-printing.
-}
instance Show Variable where
  showsPrec v x r = "(x" ++ (show $ fromEnum x) ++ ")" ++ r


{-
  All variables.
-}
allVariables = [Variable Real 0 0 0 .. Variable Imag (vectorLength - 1) (basisWidth - 1) (numBases - 1)]




{-
  A Sonomial is a Variable raised to an integral exponent.
-}
data Sonomial = Sonomial Variable Int deriving (Eq)


{-
  Sonomial instantiates Enum for easy generation.
-}
instance Enum Sonomial where
  fromEnum (Sonomial v n) = (n - 1) * (length allVariables) + (fromEnum v)

  toEnum x = Sonomial v n
             where n = 1 + (div x (length allVariables))
                   v = toEnum $ mod x (length allVariables)


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
  Integer <-> Monomial conversion
-}
monomial2Int' :: Int -> [Sonomial] -> Int
monomial2Int' d ((Sonomial v n) : sT) = if   Data.List.null sT
                            then h
                            else h + (length $ allSonomials d) * (monomial2Int' d sT)
                            where h = if   n == 0
                                      then 0
                                      else 1 + fromEnum (Sonomial v n)

monomial2Int :: Int -> Monomial -> Int
monomial2Int d (Monomial s) = (monomial2Int' d $ s ++ (replicate (d - (length s)) (Sonomial (toEnum 0) 0))) - 1


{-
  Monomial instantiates Show for pretty-printing.
-}
instance Show Monomial where
  showsPrec v (Monomial (xH : xT)) r = if   Data.List.null xT
                                       then (show xH) ++ r
                                       else (show xH) ++ " " ++ (showsPrec v (Monomial xT) r)


{-
  Monomial instantiates Eq to provide equality under permutations.
-}
instance Eq Monomial where
  (Monomial xL) == (Monomial yL) =    (all (\ x -> any (== x) yL) xL)
                                   && (all (\ y -> any (== y) xL) yL)


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
monomialMultiply m0 (Monomial (m1H : m1T)) = if   Data.List.null m1T
                                             then monomialMultiply' m0 m1H
                                             else monomialMultiply (monomialMultiply' m0 m1H) (Monomial m1T)


{-
  Multiplicative inverse of a Monomial.
-}
monomialInverse :: Monomial -> Monomial
monomialInverse (Monomial m) = Monomial $ Data.List.map sonomialInverse m


{-
  Given a monomial m and a sonomial s, find a sonomial t such that m contains (s t).
-}
monomialMatch' :: Monomial -> Sonomial -> Sonomial
monomialMatch' (Monomial m) (Sonomial v n) = if   Data.List.null r
                                             then sonomialInverse (Sonomial v n)
                                             else Sonomial v (n' - n)
                                             where r = Data.List.filter (\ (Sonomial w l) -> w == v) m
                                                   Sonomial v' n' = head r


{-
  Given monomials x and y, find a monomial m such that x = (m y).
-}
monomialMatch :: Monomial -> Monomial -> Monomial
monomialMatch (Monomial x) (Monomial y) = Monomial [monomialMatch' (Monomial x) s | s <- y]


{-
  Generate all monomials of degree d or less.
-}
allMonomials'' :: Int -> Int -> [Monomial]
allMonomials'' v 0 = [Monomial [Sonomial (allVariables !! v) 1]]
allMonomials'' v d = [ monomialMultiply' m (Sonomial (allVariables !! v) 1) | m <- nm ]
                     where nm = concat [allMonomials'' w (d - 1) | w <- [v .. (length allVariables) - 1]]

allMonomials' :: Int -> [Monomial]
allMonomials' d = concat $ Data.List.map (\ v -> allMonomials'' v (d - 1)) [0 .. (length allVariables) - 1]

allMonomials :: Int -> [Monomial]
allMonomials d = concat $ Data.List.map allMonomials' [1 .. d]


{-
  Convert a Monomial to a unique integer.
-}
--monomial2Int :: Monomial -> Int
--monomial2Int (





{-
  A Polynomial is a constant and a list of Monomials with coefficients.
-}
data Polynomial = Polynomial Float [(Float, Monomial)]


{-
  Polynomial instantiates Show for pretty-printing.
-}
instance Show Polynomial where
  showsPrec v (Polynomial c ((k, m) : mT)) r = if   Data.List.null mT
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
polynomialAdd (Polynomial c0 m0) (Polynomial c1 ((a, m) : m1T)) = if   Data.List.null m1T
                                                                  then polynomialAdd' (Polynomial (c0 + c1) m0) a m
                                                                  else polynomialAdd (polynomialAdd' (Polynomial c0 m0) a m) (Polynomial c1 m1T)


{-
  Multiply a Polynomial by a constant.
-}
polynomialMultiply''' :: Polynomial -> Float -> Polynomial
polynomialMultiply''' (Polynomial c p) c' = Polynomial (c * c') $ Data.List.map (\ (k, m) -> (k * c', m)) p


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
polynomialMultiply x (Polynomial c p) = if   Data.List.null p
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
polynomialCoef (Polynomial c p) m' = if   Data.List.null r
                                     then 0
                                     else fst (head r)
                                     where r = Data.List.filter (\ (k, m) -> m == m') p

{-
  Remove terms from a polynomial that have coefficient zero.
-}
polynomialStrip :: Polynomial -> Polynomial
polynomialStrip (Polynomial c p) = Polynomial c $ Data.List.filter (\ (k, m) -> k /= 0) p

