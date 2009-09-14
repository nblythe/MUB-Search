{-
  Generate a list of unique standardized bases from the set of fundamental neighbors on the
  orthogonality graph.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Binary
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
  Decode a job ID j into group selections, where there are c groups and
  m groups per job.
-}
decodeJob' c (g, j) = (mod j c, div j c)
decodeJob c m j = Prelude.map fst $ take m $ tail $ iterate (decodeJob' c) (0, j)


{-
  Bases <d> <n> <m> <k> <j> <fNeighbors> <fBases>

  Dimension d.
  nth roots of unity.
  Bases contain m orthogonal vectors.
  Neighbor relations are split into groups of k.
  Job ID j.
  Neighbor relations read from fNeighbors.
  Bases written to fBases.
-}
main = do
  d : (n : (m : (k : (j : (fNeighbors : (fBases : argsT)))))) <- getArgs

  {-
    Read adjacency relations.
  -}
  putStr ("Reading orthogonality fundamental neighbors from " ++ fNeighbors ++ ".\n")
  adjOrth <- decodeFile fNeighbors :: IO (Set Int)
  putStr ("Read " ++ (show $ size adjOrth) ++ " fundamental neighbors.\n")

  {-
    Job details.
  -}
  let c = (div (size adjOrth) (read k))
          + if   rem (size adjOrth) (read k) == 0
            then 0
            else 1
  let jSel = decodeJob c ((read m) - 1) (read j)
  let jGroups' g = fromList [(elems adjOrth) !! i | i <- [iStart g .. iEnd g]]
                   where iStart g = (jSel !! g) * (read k)
                         iEnd g   = if   iEnd' > (size adjOrth) - 1
                                    then (size adjOrth) - 1
                                    else iEnd'
                                    where iEnd' = ((jSel !! g) + 1) * (read k) - 1
  let jGroups = Prelude.map jGroups' [0 .. (read m) - 2]
  let jAdjOrth = unions jGroups

  putStr ("Job ID " ++ j ++ " selects groups " ++ (show jSel) ++ " (each from " ++ (show c) ++ ").\n")
  putStr ("Selected group sizes: " ++ (show $ Prelude.map size jGroups) ++ ".\n")
  putStr ("Total fundamental neighbors selected: " ++ (show $ size jAdjOrth) ++ ".\n")

  {-
    Generate the orthogonality graph and find root cliques (standardized bases).
  -}
  let g = graph (read d, read n) jAdjOrth
  let c = rootcliques g (read m)

  {-
    Eliminate equivalents under row-permutations.
  -}
  let c' = uniqueBases c
  print $ c'


  {-
    Store to disk.
  -}
  putStr ("Writing bases to " ++ fBases ++ "...\n")
  encodeFile fBases c'
  putStr ("There are " ++ (show $ length c') ++ " unique bases.\n")
  putStr ("Done.\n")

