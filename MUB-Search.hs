
import Data.List
import Data.Binary
import Ix
import System.IO
import System(getArgs)

import GenVectorTable12
--import GenVectorTable24
import GenOrthGraph
import FindCliques

main = do
  arg_vlut : (arg_clut : arg_t) <- getArgs

  vlut <- decodeFile arg_vlut :: IO [[Bool]]
--  let vlut = GenVectorTable12.vec_table

--  olut <- decodeFile arg_olut :: IO [[Int]]
  let olut = orthLUT (6, 12) vlut

--  print (subsetIndex (range (0, (length olut) - 1)) [1, 2, 3, 4, 5, 6])
  print (adjVertsMap olut !! 1)
--  print (set2index olut [942])
--  print (adjVertsL olut [942])
--  print (intersect (adjVertsL olut [0]) (adjVertsL olut [942]))
--  encodeFile arg_clut (allAdjSets olut 6 0)
--  print (allAdjSets olut 6 0)
--  print $ length (allAdjSets olut 2 0)

{-
main = do
  arg0 : (arg1 : (arg2 : argT)) <- getArgs
  lut <- decodeFile  arg0 :: IO [[Int]]

  let d = (read arg2 :: Int)

  print (succAdjSets lut [[0]])
--  print (allCliques lut d)
--  print (allAdjSets lut d 0)

--  encodeFile arg1 (allCliques lut (read arg2 :: Int))


{-
  ./GenOrthGraph [infile] [outfile]

  infile:  look-up table of vector orthogonality
  out: target filename for adjacency matrix
-}
main = do
  arg0 : (arg1 : argT) <- getArgs
  lut <- decodeFile  arg0 :: IO [[Bool]]

  putStr ((show $ m) ++ " nodes.\n")
  putStr ((show $ length (allOrthIndices lut)) ++ " edges per node.\n")
  putStr ((show $ m * (length (allOrthIndices lut))) ++ " edges total.\n")

  encodeFile arg1 (orthLUT lut)



{-
  ./GenVectorTable12 [outfile]

  outfile: target filename for vector table.
-}
main = do
  argH : argT <- getArgs
  encodeFile argH vec_table


{-
  ./GenVectorTable24 [outfile]

  outfile: target filename for vector table.
-}
main = do
  argH : argT <- getArgs
  encodeFile argH vec_table

-}
