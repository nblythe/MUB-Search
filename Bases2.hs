{-
  Generate a list of unique standardized bases from the list of fundamental
  neighbors on the orthogonality graph.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)


import Data.List (genericIndex, intersperse, map)

import Graph2
import Magic2


{-
  Bases <d> <n> <fOrth> <m> <s> <j>

  Dimension d.
  nth roots of unity.
  adjacencies read from fOrth (or standard input if fOrth = '-')
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
    Read adjacency relations from file or standard input.
  -}
  adj' <- if   sOrth == "-"
          then getContents
          else readFile sOrth
  let adj = Data.List.map read (lines adj') :: [Integer]
  let nE = toInteger $ length adj
  let nJ = (div nE s) + (if mod nE s == 0 then 0 else 1)


  {-
    Specify the jobs this process will work on (a selection of the adjacency
    relations that we will use as candidates for our "first" edge).
  -}
  let jobs = [j * s .. min ((j + 1) * s - 1) (nE - 1)]
  let qs | s <= 0       = [ [0] ]
         | s > nE = error (   "Job size greater than number of adjacency"
                           ++ " relations (" ++ (show nE) ++ ")")
         | j >= nJ = error (   "Job index greater than maximum index for"
                            ++ " this job size (" ++ (show nJ) ++ ")")
         | otherwise    = [ [0, genericIndex adj i] | i <- jobs ]


  {-
    Find cliques that contain one of this job's cliques.
  -}
  let qs' = if   m < 2
            then error "Cliques of size < 2 are boring"
            else cliques (d, n) adj m qs


  {-
    Output the list in CSV format.
  -}
  let ps = map (\ q -> concat $ intersperse "," $ map show q) qs'
  sequence_ $ map putStrLn ps
