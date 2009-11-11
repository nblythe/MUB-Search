{-
  TODO

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import System.IO
import Data.Binary
import Data.Set

{-
  DumpBinaryAdjacencies <fNeighbors>
-}
main = do
  fNeighbors : argsT <- getArgs

  adjOrth <- decodeFile fNeighbors :: IO (Set Int)
  sequence_ $ Prelude.map (putStrLn . show) $ elems adjOrth

