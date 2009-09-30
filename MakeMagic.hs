{-
  TODO

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import System.IO
import Data.Binary
import Data.ByteString
import Data.ByteString.Lazy

import Magic


{-
  MakeMagic <d> <n> <fMagic>

  Dimension d.
  nth roots of unity.
  Vectors written to fMagic.
-}
main = do
  d : (n : (fMagic : argsT)) <- getArgs


  {-
    All vectors from magic numbers.
  -}
  let nD = read d :: Int
  let nN = read n :: Int
  let vecs = Prelude.map (magic2vec (nD, nN)) [0 .. (nD ^ (nN - 1)) - 1]
  let lbs = Prelude.map encode vecs

  fp <- openFile fMagic WriteMode
  let fw = Prelude.map (Data.ByteString.Lazy.hPut fp) lbs
  sequence fw
  hClose fp

