{-
  Convert a file of scalars on a vector space graph to a permutation-free
  sorted file of scalars.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List


{-
  Predicate indicating if a rank 1 scalar b is a permutation of a rank 1 scalar a.
-}
eq1 :: Integer -> Integer -> Bool
eq1 a b = a == b


{-
  Predicate indicating if a rank 2 scalar a is a permutation of a rank 2 scalar b.
-}
eq2 :: [Integer] -> [Integer] -> Bool
eq2 a b = (sort a) == (sort b)


{-
  Predicate indicating if a rank 3 scalar a is a permutation of a rank 3 scalar b.

  May report a false negative.
-}
eq3 :: [[Integer]] -> [[Integer]] -> Bool
eq3 a b = (s a) == (s b)
          where s = sort . transpose . sort . transpose . (sortBy o)
                o u v | u' < v'  = LT
                      | u' > v'  = GT
                      | u' == v' = GT
                        where u' = sort u
                              v' = sort v


{-
  Predicate indicating if a rank 4 scalar a is a permutation of a rank 4 scalar b.

  May report a false negative.
-}
eq4 :: [[[Integer]]] -> [[[Integer]]] -> Bool
eq4 a b = all t a
          where t x = any (eq3 x) b


{-
  Simplify <f> <r>

  Scalars read from f or standard input if f is "-".
  Scalars are rank r.
-}
main = do
  {-
    Command line arguments.
  -}
  f : sR : argsT <- getArgs
  let r = read sR :: Integer


  {-
    Read scalars, remove permutations, write scalars
  -}
  ts <- if   f == "-"
        then getContents
        else readFile f
  let ts' | r == 1 = map  show         $ nubBy eq1 $ (map read (lines ts) :: [Integer])
          | r == 2 = map (show . reverse . sort) $ nubBy eq2 $ (map read (lines ts) :: [[Integer]])
          | r == 3 = map (show . reverse . sort) $ nubBy eq3 $ (map read (lines ts) :: [[[Integer]]])
          | r == 4 = map (show . reverse . sort) $ nubBy eq4 $ (map read (lines ts) :: [[[[Integer]]]])
          | otherwise = error "Invalid rank"
  sequence_ $ map (putStrLn) ts'

