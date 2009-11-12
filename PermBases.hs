{-
  Generate all permutations of a file of bases.

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
  PermBases <d> <n> <sIn>

  Bases read from sIn.
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sN : (sIn : argsT)) <- getArgs
  let d = read sD :: Integer
  let n = read sN :: Integer


  {-
    Read bases.
  -}
  bases <- readFile sIn
  let bases' = map (read . fixline) (lines bases) :: [[Integer]]


  {-
    Expand to matrices and generate all row permutations.
  -}
  let mats = map ((map (0 :)) . (magics2vecs (d, n))) bases'
  let mats' = (nub . (concatMap permuteAllL)) mats


  {-
    Contract to bases and output.
  -}
  let bases'' = map ((vecs2magics (d, n)) . (map tail)) mats'
  sequence_ $ map (putStrLn . concat . (intersperse ",") . (map show)) bases''

