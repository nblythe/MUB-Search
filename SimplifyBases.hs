{-
  Convert a file of bases to a permutation-free sorted file of bases.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List
import Perms
import Magic

{-
  A string without wrapping square brackets, if present.
-}
fixline s = if  (s !! 0) == '['
            then s
            else '[' : s ++ "]"


{-
  Predicate indicating if a matrix b is a permutation of a matrix a.
-}
arePerms a b = any (== b) (permuteAll a)


{-
  SimplifyBases <d> <n> <fIn>

  Bases read from fIn.
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sN : (fIn : argsT)) <- getArgs
  let d = read sD :: Integer
  let n = read sN :: Integer


  {-
    Read bases.
  -}
  bases' <- readFile fIn
  let bases = map (sort . read . fixline) (lines bases') :: [[Integer]]


  {-
    Expand to matrices and remove permutations.
  -}
  let mats = map ((magics2vecs (d, n)) . tail . sort) bases
  let mats' = nubBy arePerms mats


  {-
    Contract to sorted lists (and sort the list of lists too) and output.
  -}
  let bases' = sort $ map ((0 :) . (vecs2magics (d, n))) mats'
  sequence_ $ map (putStrLn . ("[" ++) . (++ "]") . concat . (intersperse ",") . (map show)) bases'

