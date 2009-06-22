{-
  Set of orthonormal bases
  2009 Nathan Blythe, Dr. Oscar Boykin


-}

--module BSet where

import Data.List
import Data.Binary
import Ix
import System.IO
import System(getArgs)

import ExtRat
import ExtCpx
import Perms
import ListGen


{-
  Dimension we're working in.
-}
d :: Int
d = 6
n :: Int
n = 12
m :: Int
m = n^(d - 1)


{-
  Given a set s of orthogonal vectors, find the set of all vectors orthogonal
  to each vector in s.
-}
orthVecs lut (sH : sT) = if    (null sT)
                         then  z
                         else  intersect z (orthVecs lut sT)
                         where z = lut !! sH


{-
  Construct all successor sets to a set s of orthogonal vectors.  A successor
  set is a set that contains all vectors in the original, plus one additional
  orthogonal vector.
-}
succOrthSet lut s = map (\x -> s ++ [x]) (orthVecs lut s)


{-
  Given a list l of sets of orthogonal vectors, construct the successor
  list.  A successor list is a list that contains all successor sets of
  each set in the original list.
-}
succOrthSets lut [] = []
succOrthSets lut (lH : lT) = (succOrthSet lut lH) ++ (succOrthSets lut lT)


{-
  Given an orthogonal vector x, construct all sets of d orthogonal
  vectors that each include x.
-}
allOrthSets lut x = (iterate (succOrthSets lut) [[x]]) !! 2


{-
  All orthogonal bases in dimension d.
-}
allBases lut = map (allOrthSets lut) (range(0, m - 1))


main = do
  arg0 : (arg1 : argT) <- getArgs
  lut <- decodeFile  arg0 :: IO [[Int]]

--  print $ length (allOrthSets lut 0)

  encodeFile arg1 (allBases lut)

