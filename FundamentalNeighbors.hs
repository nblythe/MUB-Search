{-
  Given d and n, find all vectors of the form [1, x_0, x_1, ... x_{d - 2}], where
  x_i is a 2^nth root of unity, that are orthogonal or unbiased to the vector [1, 1, ... 1].

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Set
import Data.Binary
import Ratio
import Data.List
import Data.Maybe

import Magic
import SublistPred
import Cyclotomic
import Perms


{-
  Sum definition without using zero.
-}
rsum :: (Num a) => [a] -> a
rsum (h : t) = foldl (+) h t


{-
  Predicate that determines if a vector is orthogonal to the unity vector.
-}
pOrth :: Integer -> Rational -> [Cyclotome] -> Bool
pOrth p e x = (snd $ head t) <= e
              where s = (cycloOne p) + (rsum x)
                    a = [boundMag2 (2 * k) s | k <- [1 ..]]
                    t = Prelude.filter (\ b -> ((fst b) > e) || ((snd b) <= e)) a


{-
  Predicate that determines if a vector is unbiased to the unity vector.
-}
pBias :: Integer -> Rational -> [Cyclotome] -> Bool
pBias p e x = (snd $ head t) <= e
              where s = (cycloOne p) + (rsum x)
                    a = [tupleAdjust (boundMag2 (3 * k) s) | k <- [1 ..]]
                    tupleAdjust (f, s) = (abs(6 - f), abs(6 - s))
                    t = Prelude.filter (\ b -> ((fst b) > e) || ((snd b) <= e)) a


{-
  FundamentalNeighbors d p e <fOrth> <fBias>
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sP : (sE : (fOrth : (fBias : argsT)))) <- getArgs
  let d = read sD :: Integer
  let p = read sP :: Integer
  let e = read sE :: Rational
  let n = 2^p

  putStr ("Working in dimension " ++ sD ++ ", " ++ (show n) ++ "th roots of unity.\n")
  putStr ("Epsilon = " ++ (show $ fromRational e) ++ ".\n")
  putStr ("Will write orthogonality adjacency relations to " ++ fOrth ++ ".\n")
  putStr ("Will write unbiasedness adjacency relations to " ++ fBias ++ ".\n")


  {-
    The 2^nth roots of unity.
  -}
  let roots = rootsOfUnity p


  {-
    All vectors of roots of unity, unique under permutations, that are orthogonal to the unity vector.
  -}
  let vecsOrth = Prelude.map lookup rootsOrth
                 where rootsOrth = sublistPred (pOrth p e) (d - 1) roots
                       lookup = Prelude.map (\ x -> fromJust (findIndex (x ==) roots))


  {-
    Put the permutations in and convert them all to magic numbers.
  -}
  let allVecsOrth = fromList . concat $ Prelude.map permuteAllL vecsOrth
  let adjOrth = Data.Set.map (vec2magic (fromInteger d, fromInteger n)) allVecsOrth


  {-
    All vectors of roots of unity, unique under permutations, that are unbiased to the unity vector.
  -}
  let vecsBias = Prelude.map lookup rootsOrth
                 where rootsOrth = sublistPred (pBias p e) (d - 1) roots
                       lookup = Prelude.map (\ x -> fromJust (findIndex (x ==) roots))


  {-
    Put the permutations in and convert them all to magic numbers.
  -}
  let allVecsBias = fromList . concat $ Prelude.map permuteAllL vecsBias
  let adjBias = Data.Set.map (vec2magic (fromInteger d, fromInteger n)) allVecsBias


  {-
    File IO.
  -}
  encodeFile fOrth adjOrth
  encodeFile fBias adjBias


  putStr ("Found " ++ (show $ size adjOrth) ++ " orthogonality adjacencies.\n")
  putStr ("Found " ++ (show $ size adjBias) ++ " unbiasedness adjacencies.\n")

