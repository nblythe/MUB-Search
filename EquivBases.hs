{-
  Determine if two files of bases are equivalent under permutations.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Perms

eperm a b = any (== b) (permuteAll a)

inperm l a = any (eperm a) l


{-
  EquivBases <f1> <f2>
-}
main = do
  {-
    Command line arguments.
  -}
  f1 : f2 : _ <- getArgs


  {-
    Read bases.
  -}
  bases1' <- readFile f1
  let bases1 = map read (lines bases1') :: [[[Integer]]]
  bases2' <- readFile f2
  let bases2 = map read (lines bases2') :: [[[Integer]]]


  {-
    Remove all-0 vector.
  -}
  let mats1 = map (tail . sort) bases1
  let mats2 = map (tail . sort) bases2


  {-
    Check that each basis in the first file has a permutation in the second file.
  -}
  print $ all (inperm mats2) mats1

