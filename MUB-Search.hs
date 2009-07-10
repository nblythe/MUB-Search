

import Data.Binary

import System.IO
import System(getArgs)

import Graph
--import Cliques
import Magic
import Perms

import Data.Set


main = do
--  arg_vlut : (arg_clut : arg_t) <- getArgs

  orthF <- decodeFile "orth12.bin" :: IO [Int]
  biasF <- decodeFile "unbias12.bin" :: IO [Int]

--  vlut <- decodeFile arg_vlut :: IO [[Bool]]
--  let vlut = GenVectorTable12.vec_table

  --let t = [fromList [1, 2, 3], fromList [0, 2, 3], fromList [0, 1], fromList [0, 1]]
  --let v = rootcliques t 3
  --print v

  let g = graph (6, 12) (fromList orthF)
  let s = rootcliques g 6
  print $ take 10 s
  --encodeFile "cliques.bin" $ s

  --let b = orthGraphD (6, 12) biasF
  --let t = rootcliques b 7
  --print $ take 1 t

  --let bases = map (map (magic2vec (6, 12))) s
  --print bases

  --let ebases = map (map (0 :)) bases

