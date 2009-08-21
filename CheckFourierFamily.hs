{-
  Determine how many bases (from a file) are not in the Fourier family of bases.
  This is a lower bound.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Binary
import Data.List
import Data.Set

import Magic
import Perms


{-
  Determine if a basis is in "Fourier family form", as below.

  | . 4 . 8 . |
  | . 8 . 4 . |
  | 6 0 6 0 6 |
  | . 4 . 8 . |
  | . 8 . 4 . |

  Note that this isn't sufficient to determine if a basis is in the Fourier family, but
  if a basis is not in this form, then it's definitely NOT in the Fourier family.
-}
isFourierFamilyForm :: [[Int]] -> Bool
isFourierFamilyForm b =    ((b !! 0) !! 1 == 4) && ((b !! 0) !! 3 == 8)
                        && ((b !! 1) !! 1 == 8) && ((b !! 1) !! 3 == 4)
                        && ((b !! 2) !! 0 == 6) && ((b !! 2) !! 1 == 0) && ((b !! 2) !! 2 == 6) && ((b !! 2) !! 3 == 0) && ((b !! 2) !! 4 == 6) 
                        && ((b !! 3) !! 1 == 4) && ((b !! 3) !! 3 == 8)
                        && ((b !! 4) !! 1 == 8) && ((b !! 4) !! 3 == 4)


{-
  Is a basis definitely not in the Fourier family?
-}
isntFourierFamily :: [[Int]] -> Bool
isntFourierFamily b = not $ any isFourierFamilyForm (permuteAll b)


{-
  CheckFourierFamily <n> <d> <bases-file>
-}
main = do
  {-
    Command line arguments.
  -}
  arg_n : (arg_d : (arg_bases : arg_t)) <- getArgs


  {-
    Read a file of standardized bases as sets of sets of magic numbers, and convert to a
    list of matrices, removing the fundamental vector from each basis.
  -}
  f <- decodeFile arg_bases :: IO (Set (Set Int))
  let bset = Data.Set.map deleteMin f
  let bset' = Data.Set.map (\ x -> elems $ magics2vecs ((read arg_n), (read arg_d)) x) bset


  {-
    Find all bases that can't possibly be in the Fourier family.
  -}
  let notFourier = Data.Set.filter isntFourierFamily bset'


  {-
    We only have the Fourier family defined for dimension 6.
  -}
  putStr $ if   (read arg_d) == 6
           then ("Read " ++ (show $ size f) ++ " bases.\n"
                 ++ "At least " ++ (show $ size notFourier) ++ " bases are not in the Fourier family.\n")
           else ("Currently only dimension 6 is supported.\n")

