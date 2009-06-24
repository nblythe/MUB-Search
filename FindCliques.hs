{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module FindCliques where

import Data.List
import Data.Maybe
import Ix
import System.IO
import System(getArgs)


{-
  Given a set s of adjacent vertices, find the set of all vertices adjacent
  to each vertex in s.
-}
adjVerts lut (sH : sT) = if    (null sT)
                         then  z
                         else  intersect z (adjVerts lut sT)
                         where z = lut !! sH


{-
  The powerset of a set x.
-}
powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
                where xss = powerset xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)



adjVertsL' :: [[Int]] -> [Int] -> [Int]
adjVertsL' lut []        = []
adjVertsL' lut (xH : xT) = if   (null xT)
                           then lut !! xH
                           else intersect (lut !! xH) (adjVertsL lut xT)

set2index lut s = fromJust (findIndex (s ==) (powerset (range (0, (length lut) - 1))))

adjVertsMap :: [[Int]] -> [[Int]]
adjVertsMap lut = map (adjVertsL' lut)
                      (powerset (range (0, (length lut) - 1)))

adjVertsL :: [[Int]] -> [Int] -> [Int]
adjVertsL lut s = (adjVertsMap lut) !! (set2index lut s)


{-
  Construct all successor sets to a set s of adjacent vertices.  A successor
  set is a set that contains all vertices in the original, plus one additional
  adjacent vertex.
-}
succAdjSet lut s = map (\x -> s ++ [x]) (adjVerts lut s)


{-
  Given a list l of sets of adjacent vertices, construct the successor
  list.  A successor list is a list that contains all successor sets of
  each set in the original list.
-}
succAdjSets lut [] = []
succAdjSets lut (lH : lT) = (succAdjSet lut lH) ++ (succAdjSets lut lT)


{-
  Given a vertex x, construct all sets of d adjacent vertices that each
  include x.
-}
allAdjSets lut d x = (iterate (succAdjSets lut) [[x]]) !! (d - 1)


{-
  All cliques of size d on the graph.
-}
allCliques lut d = map (allAdjSets lut d) (range(0, (length lut) - 1))

