{-
  Given an adjacency matrix for a graph, find cliques of a particular size.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

import Data.List
import Data.Binary
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
  Given an vertex x, construct all sets of d adjacent vertices that each
  include x.
-}
allAdjSets lut d x = (iterate (succAdjSets lut) [[x]]) !! (d - 1)


{-
  All cliques of size d on the graph.
-}
allCliques lut d = map (allAdjSets lut d) (range(0, (length lut) - 1))


{-
  ./FindCliques [infile] [outfile] [size]

  infile:  file containing adjacency matrix.
  outfile: target file for cliques.
  size:    size of the cliques to find.
-}
main = do
  arg0 : (arg1 : (arg2 : argT)) <- getArgs
  lut <- decodeFile  arg0 :: IO [[Int]]

  encodeFile arg1 (allCliques lut (read arg2 :: Int))

