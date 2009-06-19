{-
  Set of orthonormal bases
  2009 Nathan Blythe, Dr. Oscar Boykin


-}

--module BSet where

import Data.List
import Data.Binary
import Ix
import System.IO
import System(getArgs)

import ExtRat
import ExtCpx
import Perms
import ListGen


{-
  Dimension we're working in.
-}
d :: Int
d = 6
n :: Int
n = 12
m :: Int
m = n^(d - 1)

{-
  Compute a vector's magic number.
-}
vec2magic :: [Int] -> Int
vec2magic []        = 0
vec2magic (xh : xt) = xh * n^(d - (length xt) - 2) + (vec2magic xt)


{-
  Compute a magic number's vector.
-}
magic2vec' x y = mod (div x (n^y)) n
magic2vec :: Int -> [Int]
magic2vec x = map (magic2vec' x) (range(0, d - 2))


{-
  Add two vectors.
-}
addVecs :: [Int] -> [Int] -> [Int]
addVecs a b = zipWith (\x y -> mod (x + y) (length a)) a b


{-
  Determine if two vectors are orthogonal by referring
  to a lookup table.
-}
--fetchLUT lut x y = lut !! fromInteger (vec2magic (addVecs x y))
--areOrthLUT lut x y = (fetchLUT lut x y) !! 0

areOrth lut x y = (lut !! (mod (x + y) m)) !! 0

{-
  All basis vectors.
-}
allBasisVectors = constructAllLists (d - 1) (range(0, n - 1))

main = do
  argH : argT <- getArgs
  lut <- decodeFile  argH :: IO [[Bool]]
--  print (findIndices (== [True, False]) lut)
  print $ (length (filter (areOrth lut 0) (range(0, m - 1)) ))




{-
isOrthBasis :: [[Integer]] -> Bool
isOrthBasis (x : y : ys) = if null ys then areOrthVectors x y
                           else if areOrthVectors x y then isOrthBasis (y : ys)
                                else False

-}



{-
{-
  All bases.
-}
allBases :: Integer -> Integer -> [[[Integer]]]
allBases d r = constructBigList d (allBasisVectors d r)

{-
  All orthonormal bases.
-}
allOrthBases :: Integer -> Integer -> [[[Integer]]]
allOrthBases d r = filter isOrthBasis (allBases d r)

{-
  All unique orthonormal bases.
-}
allUniqueOrthBases :: Integer -> Integer -> [[[Integer]]]
allUniqueOrthBases d r = collectElements (\x y -> not (permUnique x y)) [] (allOrthBases d r)

{-
  All unique sets of bases.
-}
--allUniqueBasisSets :: Integer -> Integer -> Integer -> [[[[Integer]]]]
--allUniqueBasisSets d r n = constructUniqueList permEquiv n (allUniqueBases d r)


{-
  All unique basis vectors.
-}
--allBasisVectors d r = map (nthBasisVector d r) (range(0, (r^d - 1)))
--allBasisVectors d r = map (constructList d (range(0, r - 1))) (range(0, (r^d - 1)))
--allUniqueBasisVectors d r = collectElements [] (allBasisVectors d r)

--allUniqueBasisVectors d r = constructUniqueList d (range(0, r - 1))


{-
  A basis is a set of basis vectors.
-}
--data Basis = Basis [BasisVector]

{-
  Implementation of the Eq class.
  Bases are equivalent under permutations.
-}
--instance Eq Basis where
--  (Basis x) == (Basis y) = null (x \\ y)

{-
  Implementation of the Show class.
-}
--instance Show Basis where
--  showsPrec d (Basis x) r = showsPrec d x r

{-
  Construct the nth d-dimensional basis (in rth roots
  of unity) using a simple numbering system.
-}
--nthBasis :: Integer -> Integer -> Integer -> Basis
--nthBasis d r n = Basis (map (\x -> allUniqueBasisVectors !! x) (constructList


--Basis (map (\x -> mod (div n (r^x)) r) (range(0, d - 1)))

{-
  All unique bases.
-}
--allBases d r = map (nthBasisVector d r) (range(0, (r^d - 1)))
--allUniqueBases d r = collectElements [] (allBasisVectors d r)


-}
