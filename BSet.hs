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
  12th roots of unity.
-}
rZero = ExtRat 0   0 0 0 -- 0
rHalf = ExtRat 0.5 0 0 0 -- 0.5
rSqrt = ExtRat 0   0 1 0 -- sqrt(3) / 2
rOne  = ExtRat 1   0 0 0 -- 1
roots = [ ExtCpx   rOne      rZero  ,
          ExtCpx   rSqrt     rHalf  ,
          ExtCpx   rHalf     rSqrt  ,
          ExtCpx   rZero     rOne   ,
          ExtCpx (-rHalf)    rSqrt  ,
          ExtCpx (-rSqrt)    rHalf  ,
          ExtCpx (-rOne)     rZero  ,
          ExtCpx (-rSqrt)  (-rHalf) ,
          ExtCpx (-rHalf)  (-rSqrt) ,
          ExtCpx   rZero   (-rOne)  ,
          ExtCpx   rHalf   (-rSqrt) ,
          ExtCpx   rSqrt   (-rHalf) ]
cZero = ExtCpx rZero rZero

{-
  Compute the inner product of two vectors.
-}
ip :: [Integer] -> [Integer] -> ExtCpx
ip [] [] = 1
ip (xh : xt) (yh : yt) = (roots !! (fromInteger xh))
                       * (roots !! (fromInteger yh))
                       + (ip xt yt)

{-
  Determine if two vectors are orthogonal by computing
  their dot product.
-}
areOrth :: [Integer] -> [Integer] -> Bool
areOrth x y = cZero == (ip x y)

{-
  Compute a vector's magic number.
-}
vec2magic :: Integer -> [Integer] -> Integer
vec2magic n []        = 0
vec2magic n (xh : xt) = xh * n^(length xt) + (vec2magic n xt)

{-
  Add two vectors.
-}
addVecs :: [Integer] -> [Integer] -> [Integer]
addVecs a b = zipWith (\x y -> mod (x + y) (toInteger (length a))) a b

{-
  Determine if two vectors are orthogonal by referring
  to a lookup table.
-}
fetchLUT n lut x y = lut !! fromInteger (vec2magic n (addVecs x y))
--areOrthLUT n lut x y = (words (fetchLUT n lut x y)) !! 0 == "1"
areOrthLUT n lut x y = (words (fetchLUT n lut x y)) !! 0

{-
  All basis vectors.
-}
allBasisVectors :: Integer -> Integer -> [[Integer]]
allBasisVectors d r = constructAllLists d (range(0, r - 1))

main = do
  argH : argT <- getArgs
--  lut <- readFile argH
  lut <- decodeFile  argH :: IO [[Bool]]
  --print $ areOrthLUT (lines lut) ((allBasisVectors 5 12) !! 1) ((allBasisVectors 5 12) !! 2)
  print $ (length (filter (areOrthLUT 12 lut ((allBasisVectors 6 12) !! 0)) (allBasisVectors 6 12)))




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
