{-
  Generate a list of unique standardized bases from the set of fundamental neighbors on the
  orthogonality graph.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)

--import Data.Set
import Data.List


import Graph2
--import Magic


{-
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
                 else yH : Data.List.transpose yT
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
-}


{-
  Bases <d> <n> <m> <s> <j>

  Dimension d.
  nth roots of unity.
  Bases contain m orthogonal vectors.
  Process performs jobs j * s through (j + 1) * s - 1.
  If s == 0, the entire search is performed.
-}
main = do
  {-
    Command line arguments.
  -}
  dS : (nS : (mS : (sS : (jS : argsT)))) <- getArgs
  let d = read dS :: Integer
  let n = read nS :: Integer
  let m = read mS :: Integer
  let s = read sS :: Integer
  let j = read jS :: Integer


  {-
    Read adjacency relations from standard input.
  -}
  adj' <- getContents
  let adj = Data.List.map read (lines adj') :: [Integer]
  let numEdges = toInteger $ length adj
  let numJobs = (div numEdges s) + (if mod numEdges s == 0 then 0 else 1)


  {-
    Specify the jobs this process will work on (a selection of the adjacency
    relations that we will use as candidates for our "first" edge).
  -}
  let jobs = [j * s .. min ((j + 1) * s - 1) (numEdges - 1)]
  let qs | s <= 0       = [ [0] ]
         | s > numEdges = error ("Job size greater than number of adjacency relations (" ++ (show numEdges) ++ ")")
         | j >= numJobs = error ("Job index greater than maximum index for this job size (" ++ (show numJobs) ++ ")")
         | otherwise    = [ [0, genericIndex adj i] | i <- jobs ]


  {-
    Find cliques that contain one of this job's cliques.
  -}
  let qs' = cliques (d, n) adj m qs


  {-
    Output the list in CSV format.
  -}
  let ps = map (\ q -> concat $ intersperse "," $ map show q) qs'
  sequence_ $ map putStrLn ps

