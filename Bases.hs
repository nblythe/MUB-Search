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
  Bases <d> <n> <m> <fNeighbors> <fBases>
-}
main = do
  d : (n : (m : (fNeighbors : (fBases : argsT)))) <- getArgs

  {-
    Read adjacency function.
  -}
  putStr ("Reading orthogonality fundamental neighbors from " ++ fNeighbors ++ ".\n")
  adjOrth <- decodeFile fNeighbors :: IO (Set Int)
  putStr ("Found " ++ (show $ size adjOrth))


  {-
    Generate the orthogonality graph and find root cliques (standardized bases).
  -}
  let g = graph (read d, read n) adjOrth
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

