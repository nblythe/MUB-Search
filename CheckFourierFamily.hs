{-
  Count bases that could potentially be in the Fourier family of bases.
  May include false positives.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

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
isFourierFamilyForm :: [[Integer]] -> Bool
isFourierFamilyForm b =    ((b !! 0) !! 1 == 4) && ((b !! 0) !! 3 == 8)
                        && ((b !! 1) !! 1 == 8) && ((b !! 1) !! 3 == 4)
                        && ((b !! 2) !! 0 == 6) && ((b !! 2) !! 1 == 0) && ((b !! 2) !! 2 == 6) && ((b !! 2) !! 3 == 0) && ((b !! 2) !! 4 == 6) 
                        && ((b !! 3) !! 1 == 4) && ((b !! 3) !! 3 == 8)
                        && ((b !! 4) !! 1 == 8) && ((b !! 4) !! 3 == 4)


{-
  Is a basis definitely not in the Fourier family?
-}
isntFourierFamily :: [[Integer]] -> Bool
isntFourierFamily b = not $ any isFourierFamilyForm (permuteAll b)


{-
  CheckFourierFamily <f>

  Bases read from f, or standard input if f is "-".
-}
main = do
  {-
    Command line arguments.
  -}
  f : _ <- getArgs


  {-
    Read a file of bases, removing the all-0 vector.
  -}
  bases <- if   f == "-"
           then getContents
           else readFile f
  let bases' = map (tail . sort . read) (lines bases) :: [[[Integer]]]


  {-
    Find all bases that could possibly be in the Fourier family.
  -}
  let fourier = filter (not . isntFourierFamily) bases'
  putStrLn $ show (length fourier)

