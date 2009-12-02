{-
  Find bases or sets of MUBs
  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Cliques


{-
  Determine how much work this particular process will do, and form the
  starts of the cliques that it will extend.
-}
specJobs :: Integer -> Integer -> [a] -> [a] -> [[a]]
specJobs s p z l | s <= 0               = [x : z | x <- l]
                 | (s < 0) || (s > nJ)  = error ("Job size out of range (" ++ (show nJ) ++ " jobs total)")
                 | (p < 0) || (p >= nP) = error ("Process index out of range (" ++ (show nP) ++ " processes total)")
                 | otherwise            = map (\x -> (genericIndex l x) : z) [p * s .. min ((p + 1) * s - 1) (nJ - 1)]
                   where nJ = toInteger (length l)
                         nP = (div nJ s) + (if mod nJ s == 0 then 0 else 1)


{-
  MUB-Search <d> <n> <fAdj> <fSca> <r> <k> <s> <p>

  Dimension d space.
  Field of scalars has size n.
  Vectors adjacent to the 0-vector read from fAdj.
  Scalars read from fSca or standard input if fSca is "-".
  Scalars are rank r.
  Search for k-cliques.
  This process performs jobs p * s through (p + 1) * s - 1.
  If s == 0, the entire search is performed.
-}
main = do
  {-
    Command line arguments.
  -}
  d' : n' : fAdj : fSca : r' : k' : s' : p' : argsT <- getArgs
  let d = read d' :: Integer
  let n = read n' :: Integer
  let r = read r' :: Integer
  let k = read k' :: Integer
  let s = read s' :: Integer
  let p = read p' :: Integer


  {-
    Common.
  -}
  ns' <- readFile fAdj
  vs' <- if   fSca == "-"
         then getContents
         else readFile fSca


  {-
    r = 2.
  -}
  let q2 = cliques n k ns (specJobs s p [genericReplicate (d - 1) 0] vs)
           where ns = map read (lines ns') :: [[Integer]]
                 vs = map read (lines vs') :: [[Integer]]


  {-
    r = 3.
  -}
  let q3 = cliques n (k - 1) ns (specJobs s p [] vs)
           where ns = transpose $ map ((genericTake d) . repeat . read) (lines ns') :: [[[Integer]]]
                 vs = map read (lines vs') :: [[[Integer]]]


  {-
    Output.
  -}
  let s | k <= 1 = error ((show k) ++ "-cliques are boring")
        | r == 2 = map (putStrLn . show) q2
        | r == 3 = map (putStrLn . show) q3
        | otherwise = error "Invalid rank"
  sequence_ $ s

