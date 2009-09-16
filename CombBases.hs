{-
  Combine a series of files of bases into a single file.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Binary
import Data.Set

import Magic


{-
  Type definitions.
-}
type Basis = Set Int


{-
  Inflate a basis from a set of vertices to a matrix of roots.
-}
inflateBasis :: Basis -> [[Int]]
inflateBasis b = Prelude.map (0 :) b'
                 where b'  = elems $ magics2vecs (6, 12) b

{-
  Transpose a matrix.
-}
transpose :: [[Int]] -> [[Int]]
transpose []   = []
transpose [[]] = [[]]
transpose x    = if   Prelude.null (head yT)
                 then [yH]
                 else yH : transpose yT
                 where yH = Prelude.map head x
                       yT = Prelude.map tail x


{-
  Is a basis unique to all bases in a list under row permutations?
-}
isBasisUniqueToList :: [Basis] -> Basis -> Bool
isBasisUniqueToList l b = all (/= b') l'
                          where l' = Prelude.map (fromList . transpose . inflateBasis) l
                                b' = (fromList . transpose . inflateBasis) b


{-
  Produce a list of unique bases from a list of bases.
-}
uniqueBases :: [Basis] -> [Basis]
uniqueBases []        = []
uniqueBases (lH : lT) = if   Prelude.null lT
                        then [lH]
                        else if   isBasisUniqueToList r lH
                             then lH : r
                             else r
                        where r = uniqueBases lT


{-
  CombBases <fPrefix> <i> <j> <fOut>

  Filenames begin with fPrefix.
  Filenames end with integers from i to j.
  Output stored to fOut.
-}
main = do
  fPrefix : (i : (j : (fOut : argsT))) <- getArgs

  {-
    Read the bases.
  -}
  let fNames = Prelude.map (\ k -> fPrefix ++ (show k)) [(read i :: Int) .. (read j :: Int)]
  putStr ("Reading bases from from " ++ (head fNames) ++ " through " ++ (last fNames) ++ ".\n")
  aBases <- sequence $ Prelude.map (\ f -> decodeFile f :: IO [Basis]) fNames
  putStr ("Read " ++ (show . sum $ Prelude.map length aBases) ++ " bases.\n")
  
  {-
    Combine into a single list and remove equivalences.
  -}
  let uBases = uniqueBases $ concat aBases
  putStr ("Found " ++ (show . length $ uBases) ++ " unique bases.\n")

  {-
    Store to disk.
  -}
  putStr ("Writing unique bases to " ++ fOut ++ ".\n")
  encodeFile fOut uBases
  putStr ("Done.\n")

