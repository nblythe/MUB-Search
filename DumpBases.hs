{-
  Quickly print out bases in a file.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.Binary
import Data.Set

type Basis = Set Int

main = do
  fName : argsT <- getArgs

  bases <- decodeFile fName :: IO [Basis]
  sequence_ $ Prelude.map (print . elems) bases

