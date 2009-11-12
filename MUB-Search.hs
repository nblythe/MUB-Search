{-
  Find sets of MUBs amongst provided bases.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}


import System(getArgs)

import Magic
import Data.List
import Perms
import Cliques

import Voodoo


{-
  MUB-Search <d> <n> <sBias> <sBases> <m> s p

  Dimension d.
  nth roots of unity.
  Fundamental neighbors read from sBias.
  Bases read from sBases.
  Sets contain m unbiased bases.
  Process performs jobs p * s through (p + 1) * s - 1.
  If s == 0, the entire search is performed.
-}
main = do
  {-
    Command line arguments.
  -}
  dS : (nS : (sBias : (sBases : (mS : (sS : (pS : argsT)))))) <- getArgs
  let d = read dS :: Integer
  let n = read nS :: Integer
  let m = read mS :: Integer
  let s = read sS :: Integer
  let p = read pS :: Integer


  {-
    Read adjacency relations and bases.
  -}
  adj' <- readFile sBias
  let adj = map ((replicate (fromInteger d)) . read) (lines adj') :: [[Integer]]
  bases' <- readFile sBases
  let bases = map (read . ("[" ++) . (++ "]")) (lines bases') :: [[Integer]]
  let ubases = permfree (transpose . sort . transpose . magics2vecs (d, n)) bases


  {-
    There is one job per basis and this process will be working on jobs
    j * s through (j + 1) * s - 1.  Get those bases.
  -}
  let nJ = toInteger (length ubases)
  let nP = (div nJ s) + (if mod nJ s == 0 then 0 else 1)
  let jobs | s <= 0               = [[x] | x <- bases]
           | (s < 0) || (s > nJ)  = error ("Job size out of range (" ++ (show nJ) ++ "jobs total)")
           | (p < 0) || (p >= nP) = error ("Process index out of range (" ++ (show nP) ++ " processes total)")
           | otherwise            = map (\x -> [genericIndex bases x]) [p * s .. min ((p + 1) * s - 1) (nJ - 1)]


  {-
    Find sets of m mutually unbiased bases, each including one of the bases assigned to
    this process.
  -}
  let f = neighbors (magics2vecs (d, n)) (zipWith (vecDiff n)) (vecs2magics (d, n)) adj
  print $ cliques (\ x y -> (sort x) == (sort  f m jobs

