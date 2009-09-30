{-
  Generate the set of vectors orthogonal to the fundamental vector, and the set of vectors
  unbiased to the fundamental vector.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Set
import Data.Binary
import Ratio
--import Complex
import Data.List
import Data.Maybe

--import Roots12
--import Roots24
import Magic
import SublistPred
import Cyclotomic
import Perms


{-
  Sum definition without using zero.
-}
rsum :: (Num a) => [a] -> a
rsum (h : t) = if   Prelude.null t
               then h
               else h + (rsum t)


{-
  Predicate that determines if a vector is orthogonal to the unity vector.
-}
--pOrth x = 0 == 1 + (sum x)
{-pOrth e x = (compRationalRealCyclotomic24 r d) == 1
            where c = abs (1 + (sum x))
                  r = e
                  d = absRealCyclotomic24 c
-}

pOrth :: Integer -> Rational -> [Cyclotome] -> Bool
pOrth p e x = (snd $ head t) <= e
              where s = (cycloOne p) + (rsum x)
                    a = [boundMag2 (2 * k) s | k <- [1 ..]]
                    t = Prelude.filter (\ b -> ((fst b) > e) || ((snd b) <= e)) a


{-
  Predicate that determines if a vector is unbiased to the unity vector.
-}
--pBias x = 6 == abs (1 + (sum x))
{-pBias e x = (compRationalRealCyclotomic24 r d) == 1
            where c = abs (1 + (sum x))
                  r = e
                  d = absRealCyclotomic24 (c - 6)
-}

pBias :: Integer -> Rational -> [Cyclotome] -> Bool
pBias p e x = (snd $ head t) <= e
              where s = (cycloOne p) + (rsum x)
                    a = [tupleAdjust (boundMag2 (3 * k) s) | k <- [1 ..]]
                    tupleAdjust (f, s) = (abs(6 - f), abs(6 - s))
                    t = Prelude.filter (\ b -> ((fst b) > e) || ((snd b) <= e)) a


{-
  Set of dimension 6 vectors constructed from 12th roots of unity that
  are orthogonal to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
{-adjOrth12 e = Data.Set.map (vec2magic (6, 12)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots12.toInts lAsRoots
                    lAsRoots = sublistPred Roots12.roots 5 (pOrth e)


{-
  Set of dimension 6 vectors constructed from 12th roots of unity that
  are unbiased to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjBias12 e = Data.Set.map (vec2magic (6, 12)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots12.toInts lAsRoots
                    lAsRoots = sublistPred Roots12.roots 5 (pBias e)


{-
  Set of dimension 6 vectors constructed from 24th roots of unity that
  are orthogonal to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjOrth24 e = Data.Set.map (vec2magic (6, 24)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots24.toInts lAsRoots
                    lAsRoots = sublistPred Roots24.roots 5 (pOrth e)


{-
  Set of dimension 6 vectors constructed from 24th roots of unity that
  are unbiased to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjBias24 e = Data.Set.map (vec2magic (6, 24)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots24.toInts lAsRoots
                    lAsRoots = sublistPred Roots24.roots 5 (pBias e)
-}


{-
  FundamentalNeighbors d p e <fOrth> <fBias>
-}
main = do
  sD : (sP : (sE : (fOrth : (fBias : argsT)))) <- getArgs

  let d = read sD :: Integer
  let p = read sP :: Integer
  let e = read sE :: Rational
  let n = 2^p

  putStr ("Working in dimension " ++ sD ++ ", " ++ (show n) ++ "th roots of unity.\n")
  putStr ("Epsilon = " ++ (show $ fromRational e) ++ ".\n")

  let roots = rootsOfUnity p

  let vecsOrth = Prelude.map lookup rootsOrth
                 where rootsOrth = sublistPred roots (d - 1) (pOrth p e)
                       lookup = Prelude.map (\ x -> fromJust (findIndex (x ==) roots))
  let allVecsOrth = fromList . concat $ Prelude.map permuteAllL vecsOrth

  let adjOrth = Data.Set.map (vec2magic (fromInteger d, fromInteger n)) allVecsOrth

  putStr ("Writing orthogonality adjacency relations to " ++ fOrth ++ ".\n")
  encodeFile fOrth adjOrth
  putStr ("Found " ++ (show $ size adjOrth) ++ ".\n")

  {-
  let adjOrth = if   12 == (read n)
                then adjOrth12 (read e)
                else adjOrth24 (read e)

  let adjBias = if   12 == (read n)
                then adjBias12 (read e)
                else adjBias24 (read e)

  putStr ("Writing orthogonality adjacency relations to " ++ fOrth ++ ".\n")
  encodeFile fOrth adjOrth

  putStr ("Writing unbiasedness adjacency relations to " ++ fBias ++ ".\n")
  encodeFile fBias adjBias
  -}

  putStr ("Done.\n")

