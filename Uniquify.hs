{-
  Find cliques on a Butson graph

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Magic2
import Combinadics

allPerms' m = map (nat2perm m) [0 .. (numPerms m) - 1]
allPerms = map allPerms' [0 ..]

perm' x p = map (genericIndex x) p
perm x = map (perm' x) $ allPerms !! length x


isUnique [] _ = True
isUnique (h : t) x =    and (map (/= h) $ perm x)
                     && isUnique t x


uniquify' l [] = l
uniquify' l (h : t) = if  isUnique l h
                then uniquify' (h : l) t
                else uniquify' l t
uniquify x = uniquify' [] x


{-
  Uniquify <d> <n> <sAdj>

  Dimension d.
  nth roots of unity.
  Fundamental neighbors read from sAdj.
-}
main = do
  {-
    Command line arguments.
  -}
  dS : (nS : (sAdj : argsT)) <- getArgs
  let d = read dS :: Integer
  let n = read nS :: Integer


  {-
    Read adjacencies.
  -}
  adj' <- readFile sAdj
  let adj = map read (lines adj') :: [Integer]


  {-
    Expand to vectors.
  -}
  let vecs = magics2vecs (d, n) adj

  {-
    Keep only vectors unique under permutations.
  -}
  let uvecs = uniquify vecs

  {-
    Back to magic numbers and output.
  -}
  let uadj = vecs2magics (d, n) uvecs
  sequence_ $ map (putStrLn . show) uadj

