{-
  Convert a file of sets of MUBs to a permutation-free sorted file
  of sets of MUBs.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List
import Perms
import Magic


{-
  Predicate indicating if a list l1 of matrices is a permutation of a list l2
  of matrices.
-}
arePerms l1 l2 = all (\ b -> any (== b) l2') l1
                 where l2' = concatMap permuteAll l2


{-
  SimplifyMUBs <d> <n> <fIn>

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
    Read sets of bases.
  -}
  mubs' <- readFile fIn
  let mubs = map read (lines mubs') :: [[[Integer]]]


  {-
    Expand to sets of matrices and remove permutations.
  -}
  let mats = map (map ((magics2vecs (d, n)) . tail . sort)) mubs
  let mats' = nubBy arePerms mats


  {-
    Contract and output.
  -}
  let mubs' = map (map ((0 :) . (vecs2magics (d, n)))) mats'
  sequence_ $ map (putStrLn . ("[" ++) . (++ "]") . concat . (intersperse ",") . (map show)) mubs'

