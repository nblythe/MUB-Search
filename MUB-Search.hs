

import Data.Binary

import System.IO
import System(getArgs)

import OrthGraph
import Cliques
import Magic
import Perms

import Data.Set


main = do
--  arg_vlut : (arg_clut : arg_t) <- getArgs

  orthT <- decodeFile "orth12.bin" :: IO [Int]
  biasT <- decodeFile "unbias12.bin" :: IO [Int]

--  vlut <- decodeFile arg_vlut :: IO [[Bool]]
--  let vlut = GenVectorTable12.vec_table

  --let t = [fromList [1, 2, 3], fromList [0, 2, 3], fromList [0, 1], fromList [0, 1]]
  --let v = rootcliques t 3
  --print v

  let g = orthGraphD (6, 12) orthT
  let s = rootcliques g 6
  print $ take 200 s
  --encodeFile "cliques.bin" $ take 1000 s

  --let bases = map (map (magic2vec (6, 12))) s
  --print bases

  --let ebases = map (map (0 :)) bases

