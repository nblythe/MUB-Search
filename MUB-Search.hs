

import Data.Binary

import System.IO
import System(getArgs)

import OrthGraph
import Cliques
import Magic
import Perms


main = do
--  arg_vlut : (arg_clut : arg_t) <- getArgs

  orthT <- decodeFile "orth12.bin" :: IO [Int]
  biasT <- decodeFile "unbias12.bin" :: IO [Int]

--  vlut <- decodeFile arg_vlut :: IO [[Bool]]
--  let vlut = GenVectorTable12.vec_table

  let g = orthGraph (6, 12) orthT
  let gS = orthGraphS (6, 12) orthT
  --let g = orthGraphA (6, 12) orthT
  --let g = orthGraph (6, 12) vecTableOrth12
  let t = [[1, 2, 3], [0, 2, 3], [0, 1], [0, 1]]


--  print (cliques t 2)
  print $ take 100 (rootcliquesS gS 6)
--  print $ rootcliquesS gS 6

--  let bases = map (map (magic2vec (6, 12))) (rootcliques g 6)
--  let allcliques = cliques t 3

--  print bases

--  let ebases = map (map (0 :)) bases
--  let ubases = permUniqueList ebases

--  print allcliques
--  print ubases

