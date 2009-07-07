

import Data.Binary
import Data.Set
import System(getArgs)

import OrthGraph
import Magic
import Perms

n :: Int
n = 6


isOrth g x y = member y (g !! x) || member x (g !! y)

isOrthToSet g x s = all (isOrth g x) (elems s)

isOrthSet g s = if   Data.Set.null s
                then True
                else if   isOrthToSet g (findMin s) (deleteMin s)
                     then isOrthSet g (deleteMin s)
                     else False


main = do
  argO : (argF : argT) <- getArgs

  putStr ("Reading orthogonality table from \"" ++ argO ++ "\".\n")
  orthT <- decodeFile argO :: IO [Int]
  let g = orthGraphD (6, 12) orthT

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

  putStr ("Checking that all bases are orthogonal... ")
  putStr $ if   all (isOrthSet g) c
           then ("yes.\n")
           else ("no.\n")

