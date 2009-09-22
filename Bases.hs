{-
  Generate a list of unique standardized bases from the set of fundamental neighbors on the
  orthogonality graph.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import System.IO
import Data.Binary
import Data.ByteString.Lazy(hPut)
import Data.Set

import Graph
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
                          where l' = Prelude.map (fromList . Main.transpose . inflateBasis) l
                                b' = (fromList . Main.transpose . inflateBasis) b


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
  Bases <d> <n> <m> <fNeighbors> <j> <js> <fBases>

  Dimension d.
  nth roots of unity.
  Bases contain m orthogonal vectors.
  Neighbor relations read from fNeighbors.
  Job ID is j.  Job workload scaled by js.  If j == -1, the entire search is performed.
  Bases written to fBases.
-}
main = do
  d : (n : (m : (fNeighbors : (j : (js : (fBases : argsT)))))) <- getArgs


  {-
    Read adjacency relations.
  -}
  putStr ("Reading orthogonality fundamental neighbors from " ++ fNeighbors ++ ".\n")
  adjOrth <- decodeFile fNeighbors :: IO (Set Int)
  putStr ("Read " ++ (show $ size adjOrth) ++ " fundamental neighbors.\n")


  {-
    Specify the job.
  -}
  let jobs = [is .. ie]
           where is = (read j) * (read js)
                 im = (read j + 1) * (read js) - 1
                 ie = if   im < size adjOrth
                      then im
                      else (size adjOrth) - 1
  let q = if  read j == -1
          then [Data.Set.singleton 0]
          else [ fromList [0, elems adjOrth !! i] | i <- jobs ]
  putStr $ if read j == -1
                     then ("Will perform entire search.\n")
                     else ("Will perform job(s) " ++ (show jobs) ++ " of " ++ (show $ size adjOrth) ++ ".\n")


  {-
    Find fundamental cliques (standardized bases).
  -}
  putStr ("Searching for bases.\n")
  let g = graph (read d, read n) adjOrth
  let c = growcliques g q (read m)
  let c' = uniqueBases c
  putStr ("Found " ++ (show $ Prelude.length c') ++ ".\n")


  {-
    Store to disk.
  -}
--  putStr ("Writing bases to " ++ fBases ++ ".\n")
--  encodeFile fBases c'
--  putStr ("Done.\n")


  putStr ("Writing bases to " ++ fBases ++ " - experimental.\n")
  fp <- openFile fBases WriteMode
  let lbs = Prelude.map encode c'
  let fc = Prelude.map (Data.ByteString.Lazy.hPut fp) lbs
  sequence fc
  hClose fp
  putStr ("Done.\n")

