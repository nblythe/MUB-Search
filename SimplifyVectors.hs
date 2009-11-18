{-
  Convert a file of vectors to a permutation-free sorted file of bases.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Magic


{-
  Predicate indicating if a list b is a permutation of a list a.
-}
arePerms a b = (sort a) == (sort b)


{-
  SimplifyVectors <d> <n> <fIn>

  Vectors read from fIn.
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sN : (fIn : argsT)) <- getArgs
  let d = read sD :: Integer
  let n = read sN :: Integer


  {-
    Read vectors.
  -}
  vecs <- readFile fIn
  let vecs' = map read (lines vecs) :: [Integer]


  {-
    Expand to lists and remove permutations.
  -}
  let lists = magics2vecs (d, n) vecs'
  let lists' = nubBy arePerms lists


  {-
    Contract to vectors, sort, and output.
  -}
  let vecs'' = sort $ vecs2magics (d, n) lists'
  sequence_ $ map (putStrLn . show) vecs''

