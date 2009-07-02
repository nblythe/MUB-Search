

import Data.Binary

import System.IO
import System(getArgs)

import OrthGraph
import Cliques
import Magic
import Perms

import VecTables

main = do
--  arg_vlut : (arg_clut : arg_t) <- getArgs

  orthT <- decodeFile "orth12.bin" :: IO [Int]
  biasT <- decodeFile "unbias12.bin" :: IO [Int]

--  vlut <- decodeFile arg_vlut :: IO [[Bool]]
--  let vlut = GenVectorTable12.vec_table

  let g = orthGraph (6, 12) orthT
  --let g = orthGraph (6, 12) vecTableOrth12
  let t = [[1, 2, 3], [0, 2, 3], [0, 1], [0, 1]]


  --print (cliques t 3)

  let allcliques = map (map (magic2vec (6, 12))) (take 5 (rootcliques g 6))
--  let allcliques = map (map (magic2vec (6, 12))) (cliques t 3)

  let ucliques = permUniqueList allcliques

--  print allcliques
  print ucliques

