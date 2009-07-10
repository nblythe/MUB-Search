

import Data.Binary
import Data.Set
import System(getArgs)

import OrthGraph
import Magic
import Perms

n :: Int
n = 6


isAdj g x y = member x (g !! y) || member y (g !! x)
isAdjToSet g x s = all (isAdj g x) (elems s)

isMutuallyAdjSet g s = if   Data.Set.null s
                       then True
                       else if   isAdjToSet g (findMin s) (deleteMin s)
                            then isMutuallyAdjSet g (deleteMin s)
                            else False

howManyAdjToSet g [] s = 0
howManyAdjToSet g u  s = if   isAdjToSet g (head u) s
                         then 1 + (howManyAdjToSet g (tail u) s)
                         else 0 + (howManyAdjToSet g (tail u) s)

main = do
  argO : (argB : (argF : argT)) <- getArgs

  putStr ("Reading orthogonality table from \"" ++ argO ++ "\".\n")
  orthT <- decodeFile argO :: IO [Int]
  let g = orthGraphD (6, 12) orthT
  putStr ("Read " ++ (show (length orthT)) ++ " vectors orthogonal to unity.\n")

  putStr ("Reading bias table from \"" ++ argB ++ "\".\n")
  biasT <- decodeFile argB :: IO [Int]
  let b = orthGraphD (6, 12) biasT
  putStr ("Read " ++ (show (length biasT)) ++ " vectors unbiased to unity.\n")

  putStr ("Reading bases from \"" ++ argF ++ "\".\n")
  c <- decodeFile argF :: IO [Set Int]
  putStr ("Read " ++ (show (length c)) ++ " bases.\n")

  putStr ("Checking that all bases are of dimension " ++ (show n) ++ "... ")
  putStr $ if   all (\x -> 6 == size x) c
           then ("yes.\n")
           else ("no.\n")

  putStr ("Checking that all bases include the unity vector... ")
  putStr $ if   all (member 0) c
          then ("yes.\n")
          else ("no.\n")

  --putStr ("Checking that all bases are orthogonal... ")
  --putStr $ if   all (isMutuallyAdjSet g) c
  --         then ("yes.\n")
  --         else ("no.\n")

  let c0 = elems (c !! 3)
  print $ Prelude.map (magic2vec (6, 12)) c0


  --putStr ("Counting vectors unbiased to each base... ")
  --print $ howManyAdjToSet b biasT (c !! 0)

