{-
  Determine if two files of bases are equivalent under permutations.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Magic2
import Perms

eperm a b = any (== b) (permuteAll a)

inperm l a = any (eperm a) l


fixline s = if  (s !! 0) == '['
            then s
            else '[' : s ++ "]"


{-
  EquivBases d n <s1> <s2>
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sN : (s1 : (s2 : argsT))) <- getArgs
  let d = read sD :: Integer
  let n = read sN :: Integer


  {-
    Read bases.
  -}
  bases1' <- readFile s1
  let bases1 = map (read . fixline) (lines bases1') :: [[Integer]]
  bases2' <- readFile s2
  let bases2 = map (read . fixline) (lines bases2') :: [[Integer]]


  {-
    Expand bases to full matrices.
  -}
  let mats1 = map (tail . (magics2vecs (d, n))) bases1
  let mats2 = map (tail . (magics2vecs (d, n))) bases2


  {-
    Check that each basis in the first file has a row permutation in the second file.
  -}
  print $ map (inperm mats2) mats1

  --sequence_ $ map (putStrLn . show) mats1

