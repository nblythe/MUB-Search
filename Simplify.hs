{-
  Convert a file of bases to a permutation-free sorted file of bases.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Magic
import Perms


{-
  Predicate indicating if a vector b is a permutation of a vector a.
-}
eq1 :: Integer -> Integer -> Integer -> Integer -> Bool
eq1 d n a b = (s a) == (s b)
              where s = sort . (magic2vec (d, n))


{-
  Predicate indicating if a basis b is a permutation of a basis a.

  May report a false negative if a and b both contain a pair of vectors
  equivalent under permutations.
-}
eq2 :: Integer -> Integer -> [Integer] -> [Integer] -> Bool
eq2 d n a b = (s a) == (s b)
              where s = sort . transpose . sort . transpose . (sortBy o) . (magics2vecs (d, n))
                    o u v | u' < v'  = LT
                          | u' > v'  = GT
                          | u' == v' = GT
                            where u' = sort u
                                  v' = sort v


{-
  Predicate indicating if a set of MUBs b is a permutation of a set
  of MUBs a.

  May report a false negative if a and b each contain a basis that contains
  a pair of vectors equivalent under permutations.
-}
eq3 :: Integer -> Integer -> [[Integer]] -> [[Integer]] -> Bool
eq3 d n a b = all t a
              where t x = any (eq2 d n x) b


{-
  Simplify <d> <n> <r> <fIn>

  Objects are scalars (r = 1), vectors (r = 2), or matrices (r = 3).
  Objects read from fIn or standard input if fIn is "-".
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sN : (sR : (fIn : argsT))) <- getArgs
  let d = read sD :: Integer
  let n = read sN :: Integer
  let r = read sR :: Integer


  {-
    Read objects, remove permutations, write objects.
  -}
  ts <- if   fIn == "-"
        then getContents
        else readFile fIn
  let ts' | r == 1 = map show $ nubBy (eq1 d n) $ (map read (lines ts) :: [Integer])
          | r == 2 = map (show . sort) $ nubBy (eq2 d n) $ (map read (lines ts) :: [[Integer]])
          | r == 3 = map (show . sort) $ nubBy (eq3 d n) $ (map read (lines ts) :: [[[Integer]]])
  sequence_ $ map (putStrLn) ts'

