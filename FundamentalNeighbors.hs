{-
  Given d and n, find all vectors of the form [1, x_0, x_1, ... x_{d - 2}], where
  x_i is a 2^nth root of unity, that are orthogonal or unbiased to the vector [1, 1, ... 1].

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Ratio
import Data.List
import Data.Maybe

import SublistPred
import Cyclotomic
import Perms


{-
  Sum definition without using zero.
-}
rsum :: (Num a) => [a] -> a
rsum (h : t) = foldl (+) h t


{-
  Unique elements in a list.
-}
unique xs =
 let
  r = u xs 0                         -- result list
  u []     _ = []                    -- build result
  u (x:xs) n = if member x r n then u xs n
               else x:(u xs (n+1))
  member e xs     0 = False
  member y (x:xs) n = x==y || member y xs (n-1)
 in r


{-
  Union of all lists in a list.
-}
lunions :: (Eq a) => [[a]] -> [a]
lunions []       = []
lunions (h : t)  = Data.List.union (unique h) (lunions t)


{-
  Predicate that determines if a vector is orthogonal to the unity vector.
-}
pOrth :: Integer -> Rational -> [Cyclotome] -> Bool
pOrth p e x = (snd $ head t) <= e
              where s = (cycloOne p) + (rsum x)
                    a = [boundMag2 (4 * k) s | k <- [1 ..]]
                    t = filter (\ b -> ((fst b) > e) || ((snd b) <= e)) a


{-
  Predicate that determines if a vector is unbiased to the unity vector.
-}
pBias :: Integer -> Rational -> [Cyclotome] -> Bool
pBias p e x = (snd $ head t) <= e
              where s = (cycloOne p) + (rsum x)
                    a = [tupleAdjust (boundMag2 (3 * k) s) | k <- [1 ..]]
                    tupleAdjust (f, s) = (abs(6 - f), abs(6 - s))
                    t = filter (\ b -> ((fst b) > e) || ((snd b) <= e)) a


{-
  FundamentalNeighbors d p e m s j

  Dimension d.
  2^pth roots of unity.
  Approximation error e.
  Adjacency is orthogonality if m == 0.
  Adjacency is unbiasedness if m == 1.
  This process performs jobs j * s through (j + 1) * s - 1.
  If s == 0, the entire search is performed.
-}
main = do
  {-
    Command line arguments.
  -}
  sD : sP : sE : sM : sS : sJ : _ <- getArgs
  let d = read sD :: Integer
  let p = read sP :: Integer
  let e = read sE :: Rational
  let m = read sM :: Integer
  let s = read sS :: Integer
  let j = read sJ :: Integer
  let n = 2^p


  {-
    The 2^pth roots of unity.
  -}
  let roots = rootsOfUnity p


  {-
    Job size of 0 implies entire job.
  -}
  let s' | s == 0 = sublistCount (d - 1) roots
         | s >= sublistCount (d - 1) roots = error $ "job size too large (max " ++ (show $ (sublistCount (d - 1) roots) - 1) ++ ")"
         | otherwise = s
  let j' | j >= sublistJobs (d - 1) roots s' = error $ "job index too large (max " ++ (show $ (sublistJobs (d - 1) roots s') - 1) ++ ")"
         | otherwise = j


  {-
    Convert a list of roots of unity to a list of indices in the list of all roots of unity.
  -}
  let lookup = map (\ x -> toInteger $ fromJust (findIndex (x ==) roots))


  {-
    All vectors of roots of unity that are orthogonal / unbiased to the unity vector, for
    the selected job.
  -}
  let rootsOrth = genericIndex (sublistPredP (pOrth p e) (d - 1) roots s') j'
  let rootsBias = genericIndex (sublistPredP (pBias p e) (d - 1) roots s') j'


  {-
    Select the desired list of vectors and form all permutations.
  -}
  let roots = if   m == 0
              then rootsOrth
              else rootsBias
  let vecs = map lookup roots
  let allVecs = lunions $ map permuteAllL vecs


  {-
    Output the list.
  -}
  sequence_ $ map (putStrLn . show) allVecs

