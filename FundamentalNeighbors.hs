{-
  Generate the set of vectors orthogonal to the fundamental vector, and the set of vectors
  unbiased to the fundamental vector.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Set
import Data.Binary

import Roots12
import Roots24
import Magic
import SublistPred


{-
  Predicate that determines if a vector is orthogonal to the unity vector.
-}
pOrth x = 0 == 1 + (sum x)


{-
  Predicate that determines if a vector is unbiased to the unity vector.
-}
pBias x = 6 == abs (1 + (sum x))


{-
  Set of dimension 6 vectors constructed from 12th roots of unity that
  are orthogonal to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjOrth12 = Data.Set.map (vec2magic (6, 12)) (fromList lAsInts)
            where lAsInts = Prelude.map Roots12.toInts lAsRoots
                  lAsRoots = sublistPred Roots12.roots 5 pOrth


{-
  Set of dimension 6 vectors constructed from 12th roots of unity that
  are unbiased to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjBias12 = Data.Set.map (vec2magic (6, 12)) (fromList lAsInts)
            where lAsInts = Prelude.map Roots12.toInts lAsRoots
                  lAsRoots = sublistPred Roots12.roots 5 pBias


{-
  Set of dimension 6 vectors constructed from 24th roots of unity that
  are orthogonal to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjOrth24 = Data.Set.map (vec2magic (6, 24)) (fromList lAsInts)
            where lAsInts = Prelude.map Roots24.toInts lAsRoots
                  lAsRoots = sublistPred Roots24.roots 5 pOrth


{-
  Set of dimension 6 vectors constructed from 24th roots of unity that
  are unbiased to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjBias24 = Data.Set.map (vec2magic (6, 24)) (fromList lAsInts)
            where lAsInts = Prelude.map Roots24.toInts lAsRoots
                  lAsRoots = sublistPred Roots24.roots 5 pBias


{-
  FundamentalNeighbors <fOrth12> <fBias12> <fOrth24> <fBias24>
-}
main = do
  fOrth12 : (fBias12 : (fOrth24 : (fBias24 : argsT))) <- getArgs

  putStr ("Writing " ++ fOrth12 ++ "...\n")
  encodeFile fOrth12 adjOrth12

  putStr ("Writing " ++ fBias12 ++ "...\n")
  encodeFile fBias12 adjBias12

  putStr ("Writing " ++ fOrth24 ++ "...\n")
  encodeFile fOrth24 adjOrth24

  putStr ("Writing " ++ fBias12 ++ "...\n")
  encodeFile fBias24 adjBias24

  putStr ("Done.\n")

