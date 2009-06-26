{-
  Determine how many bases (from a file) could possibly be in the Fourier
  family of bases.  This is an upper bound.

  2009 Nathan Blythe, Dr. Oscar Boykin

  What we do here is try to permutate each basis into the form of the
  Fourier family of bases, considering only the constant rows and columns.
  If so, we extract the other fields which constitute two systems of
  equations, mod 12.

  If you wanted to know whether a basis was actually in the Fourier
  family you would need to see if those two systems of equation have
  solutions.  However, if this program reports a number smaller than
  the number of bases in the file then at least one basis cannot be
  in the Fourier family.
-}

import Data.List
import Data.Binary
import Data.Complex
import Data.Array
import System(getArgs)

import Perms


{-
  Remove unity vector from a basis.
-}
shrinkBasis (bH : bT) = if   (null bT)
                        then []
                        else bH : shrinkBasis bT


{-
  Determine if a basis is in "Fourier family form", as below.

  | . 4 . 8 . |
  | . 8 . 4 . |
  | 6 0 6 0 6 |
  | . 4 . 8 . |
  | . 8 . 4 . |
-}
isFourierFamilyForm b =    ((b !! 0) !! 1 == 4) && ((b !! 0) !! 3 == 8)
                        && ((b !! 1) !! 1 == 8) && ((b !! 1) !! 3 == 4)
                        && ((b !! 2) !! 0 == 6) && ((b !! 2) !! 1 == 0) && ((b !! 2) !! 2 == 6) && ((b !! 2) !! 3 == 0) && ((b !! 2) !! 4 == 6) 
                        && ((b !! 3) !! 1 == 4) && ((b !! 3) !! 3 == 8)
                        && ((b !! 4) !! 1 == 8) && ((b !! 4) !! 3 == 4)


{-
  Collect the non-constant elements from a basis in "Fourier family form".

  Resulting list is as below:
  |   0 + 12 a  ,   0 + 12 b  |
  |   4 + 12 a  ,   4 + 12 b  |
  |   6 + 12 a  ,   6 + 12 b  |
  |   8 + 12 a  ,   8 + 12 b  |
  |  10 + 12 a  ,  10 + 12 b  |
  |  14 + 12 a  ,  14 + 12 b  |
-}
getFourierFamilyFormVs b = [ [ (b !! 3) !! 2  ,  (b !! 1) !! 2 ]
                           , [ (b !! 3) !! 4  ,  (b !! 1) !! 0 ]
                           , [ (b !! 0) !! 2  ,  (b !! 4) !! 2 ]
                           , [ (b !! 3) !! 0  ,  (b !! 1) !! 4 ]
                           , [ (b !! 0) !! 4  ,  (b !! 4) !! 0 ]
                           , [ (b !! 0) !! 0  ,  (b !! 4) !! 4 ] ]


{-
  Find the variables that go with every permutation of a basis that is in
  "Fourier family form".
-}
findFourierFamilyFormVs b = map getFourierFamilyFormVs (filter isFourierFamilyForm (permuteAll b))


main = do
  arg_bases : arg_t <- getArgs

  f <- decodeFile arg_bases :: IO [[[Int]]]
  let bset = map shrinkBasis f

  print $ length (filter (\x -> (not (null (findFourierFamilyFormVs x)))) bset)

