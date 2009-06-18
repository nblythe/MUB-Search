{-
  Set of orthonormal bases
  2009 Nathan Blythe, Dr. Oscar Boykin


-}

--module BSet where

import Data.List
import Ix

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
  Determine if two vectors are orthogonal.
-}
areOrth :: [Integer] -> [Integer] -> Bool
areOrth x y = cZero == (ip x y)


{-
  All basis vectors.
-}
allBasisVectors :: Integer -> Integer -> [[Integer]]
allBasisVectors d r = constructAllLists d (range(0, r - 1))


main = do
  print $ (length (filter (areOrth ((allBasisVectors 4 12) !! 0)) (allBasisVectors 4 12)))




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



{-
  A set of bases.
-}
--data BSet = BSet [Basis]

{-
  Implementation of the Eq class.
  Sets of bases are equivalent under permutations.
-}
--instance Eq BSet where
--  (BSet x) == (BSet y) = null (x \\ y)

{-
  Implementation of the Show class.
-}
--instance Show BSet where
--  showsPrec d (BSet x) r = showsPrec d x r

-}
