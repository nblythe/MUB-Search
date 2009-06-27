{-
  Store vector tables to disk.

  2009 Nathan Blythe, Dr. Oscar Boykin

  ./Gen-VecTables <orth12> <unbias12> <orth24> <unbias24>
-}

import Data.Binary
import System(getArgs)

import VecTables

main = do
  fnOrth12 : (fnUnbias12 : (fnOrth24 : (fnUnbias24 : argsT))) <- getArgs

  putStr ("Writing " ++ fnOrth12 ++ "...\n")
  encodeFile fnOrth12 vecTableOrth12

  putStr ("Writing " ++ fnUnbias12 ++ "...\n")
  encodeFile fnUnbias12 vecTableUnbias12

  putStr ("Writing " ++ fnOrth24 ++ "...\n")
  encodeFile fnOrth24 vecTableOrth24

  putStr ("Writing " ++ fnUnbias24 ++ "...\n")
  encodeFile fnUnbias24 vecTableUnbias24

  putStr ("Done.\n")
