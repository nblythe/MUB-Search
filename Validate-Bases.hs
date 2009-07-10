

import Data.Binary
import Data.Set
import System(getArgs)

import Graph
import Magic
import Perms


isAdj g x y = member x (g !! y) || member y (g !! x)
isAdjToSet g x s = all (isAdj g x) (elems s)

isMutuallyAdjSet g s = if   Data.Set.null s
                       then True
                       else if   isAdjToSet g (findMin s) (deleteMin s)
                            then isMutuallyAdjSet g (deleteMin s)
                            else False


main = do
  argO : (argB : (argF : argT)) <- getArgs

  putStr ("Reading orthogonality table from \"" ++ argO ++ "\".\n")
  orthF <- decodeFile argO :: IO [Int]
  let g = graph (6, 12) (fromList orthF)
  putStr ("Read " ++ (show (length orthF)) ++ " vectors orthogonal to unity.\n")

  putStr ("Reading bias table from \"" ++ argB ++ "\".\n")
  biasF <- decodeFile argB :: IO [Int]
  let b = graph (6, 12) (fromList biasF)
  putStr ("Read " ++ (show (length biasF)) ++ " vectors unbiased to unity.\n")

  putStr ("Reading bases from \"" ++ argF ++ "\".\n")
  c <- decodeFile argF :: IO [Set Int]
  putStr ("Read " ++ (show (length c)) ++ " bases.\n")

  putStr ("Checking that all bases are of dimension 6... ")
  putStr $ if   all (\x -> 6 == size x) c
           then ("yes.\n")
           else ("no.\n")

  putStr ("Checking that all bases include the unity vector... ")
  putStr $ if   all (member 0) c
          then ("yes.\n")
          else ("no.\n")

  putStr ("Checking that all bases are orthogonal... ")
  putStr $ if   all (isMutuallyAdjSet g) c
           then ("yes.\n")
           else ("no.\n")


  let c0 = elems (c !! 0)
  print $ Prelude.map (magic2vec (6, 12)) c0


