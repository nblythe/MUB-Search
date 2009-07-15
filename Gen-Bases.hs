{-
  Generate a list of unique standardized bases from an orthogonality adjacency function.

  2009 Nathan Blythe, Dr. Oscar Boykin

  ./Gen-Bases <d> <n> <fAdj> <fBases>
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
inflateBasis b = Prelude.map (0 :) b''
                 where b'  = (insert 0 b)
                       b'' = elems $ magics2vecs (6, 12) b'


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
uniqueBases (lH : lT) = if   Prelude.null lT
                        then [lH]
                        else if   isBasisUniqueToList r lH
                             then lH : r
                             else r
                        where r = uniqueBases lT


{-
  Entry point.
-}
main = do
  d : (n : (fAdj : (fBases : argsT))) <- getArgs

  {-
    Read adjacency function.
  -}
  putStr ("Reading orthogonality relations from " ++ fAdj ++ ".\n")
  adjOrth <- decodeFile fAdj :: IO (Set Int)


  {-
    Generate the orthogonality graph and find root cliques (standardized bases).
  -}
  let g = graph (read d, read n) adjOrth
  let c = rootcliques g (read d)


  {-
    Eliminate equivalents under row-permutations.
  -}
  let c' = uniqueBases c


  {-
    Store to disk.
  -}
  putStr ("Writing bases to " ++ fBases ++ "...\n")
  encodeFile fBases c'
  putStr ("Done.\n")

