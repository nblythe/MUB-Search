{-
  Generate a list of unique standardized bases from the list of fundamental
  neighbors on the orthogonality graph.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)


import Data.List

import Cliques
import Magic

import Voodoo


{-
  Bases <d> <n> <sOrth> <m> <s> <j>

  Dimension d.
  nth roots of unity.
  Fundamental neighbors read from sOrth.
  Bases contain m orthogonal vectors.
  Process performs jobs j * s through (j + 1) * s - 1.
  If s == 0, the entire search is performed.
-}
main = do
  {-
    Command line arguments.
  -}
  dS : (nS : (sOrth : (mS : (sS : (jS : argsT))))) <- getArgs
  let d = read dS :: Integer
  let n = read nS :: Integer
  let m = read mS :: Integer
  let s = read sS :: Integer
  let j = read jS :: Integer


  {-
    Read adjacency relations.
  -}
  adj' <- readFile sOrth
  let adj = map read (lines adj') :: [Integer]
  let uadj = permfree (sort . magic2vec (d, n)) adj
  let nE = toInteger $ length uadj
  let nJ = (div nE s) + (if mod nE s == 0 then 0 else 1)


  {-
    Specify the jobs this process will work on (a selection of the unique
    adjacency relations that we will use as candidates for our "first" edge).
  -}
  let jobs = [j * s .. min ((j + 1) * s - 1) (nE - 1)]
  let qs | s <= 0       = map (\ a -> [a, 0]) uadj
         | s > nE = error (   "Job size greater than number of adjacency"
                           ++ " relations (" ++ (show nE) ++ ")")
         | j >= nJ = error (   "Job index greater than maximum index for"
                            ++ " this job size (" ++ (show nJ) ++ ")")
         | otherwise    = [ [0, genericIndex uadj i] | i <- jobs ]


  {-
    Find cliques that contain one of this job's cliques.
  -}
  let f = neighbors (magic2vec (d, n)) (vecDiff n) (vec2magic (d, n)) adj
  let qs' = if   m < 2
            then error "Cliques of size < 2 are boring"
            else cliques (==) f m qs


  {-
    Output the list in CSV format.
  -}
  let ps = map (\ q -> concat $ "[" ++ (intersperse "," $ map show q) ++ "]") qs'
  sequence_ $ map putStrLn ps

