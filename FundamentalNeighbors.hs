{-
  Generate the set of vectors orthogonal to the fundamental vector, and the set of vectors
  unbiased to the fundamental vector.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Set
import Data.Binary
import Ratio
import Complex

import Roots12
import Roots24
import Magic
import SublistPred
import Cyclotomic24


{-
  Predicate that determines if a vector is orthogonal to the unity vector.
-}
--pOrth x = 0 == 1 + (sum x)
pOrth e x = (compRationalRealCyclotomic24 r d) == 1
            where c = abs (1 + (sum x))
                  r = e
                  d = absRealCyclotomic24 c


{-
  Predicate that determines if a vector is unbiased to the unity vector.
-}
--pBias x = 6 == abs (1 + (sum x))
pBias e x = (compRationalRealCyclotomic24 r d) == 1
            where c = abs (1 + (sum x))
                  r = e
                  d = absRealCyclotomic24 (c - 6)


{-
  Set of dimension 6 vectors constructed from 12th roots of unity that
  are orthogonal to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjOrth12 e = Data.Set.map (vec2magic (6, 12)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots12.toInts lAsRoots
                    lAsRoots = sublistPred Roots12.roots 5 (pOrth e)


{-
  Set of dimension 6 vectors constructed from 12th roots of unity that
  are unbiased to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjBias12 e = Data.Set.map (vec2magic (6, 12)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots12.toInts lAsRoots
                    lAsRoots = sublistPred Roots12.roots 5 (pBias e)


{-
  Set of dimension 6 vectors constructed from 24th roots of unity that
  are orthogonal to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjOrth24 e = Data.Set.map (vec2magic (6, 24)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots24.toInts lAsRoots
                    lAsRoots = sublistPred Roots24.roots 5 (pOrth e)


{-
  Set of dimension 6 vectors constructed from 24th roots of unity that
  are unbiased to the unity vector (stored as magic numbers).  First element
  is assumed to be 1.
-}
adjBias24 e = Data.Set.map (vec2magic (6, 24)) (fromList lAsInts)
              where lAsInts = Prelude.map Roots24.toInts lAsRoots
                    lAsRoots = sublistPred Roots24.roots 5 (pBias e)


{-
  FundamentalNeighbors d n e <fOrth> <fBias>
-}
main = do
  d : (n : (e : (fOrth : (fBias : argsT)))) <- getArgs

  putStr ("Working in dimension " ++ d ++ ", " ++ n ++ "th roots of unity.\n")
  putStr ("Epsilon = " ++ e ++ ".\n")

  let adjOrth = if   12 == (read n)
                then adjOrth12 (read e)
                else adjOrth24 (read e)

  let adjBias = if   12 == (read n)
                then adjBias12 (read e)
                else adjBias24 (read e)

  putStr ("Writing orthogonality adjacency relations to " ++ fOrth ++ ".\n")
  encodeFile fOrth adjOrth

  putStr ("Writing unbiasedness adjacency relations to " ++ fBias ++ ".\n")
  encodeFile fBias adjBias

  putStr ("Done.\n")

