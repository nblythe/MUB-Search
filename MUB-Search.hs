{-
  MUB-Search.hs

  Search a previously-constructed table for MUBs.
-}

import Data.List
import Data.Complex
import Data.Array

{-
  Main routine: find and display all non-trivial MUBs as generated above.
-}
main = do
  print "All non-trivial MUBs:"
  print non_trivial_mubs
       print $ map length non_trivial_mubs

  putStr ("Found " ++ (show $ length non_trivial_mubs))
  putStr (" sets, the largest having " ++ (show $ maximum $ map length non_trivial_mubs))
  putStr " mutually unbiased bases.\n\n"

  putStr "All non-trivial MUBs:\n"
  putStr ((show non_trivial_mubs) ++ "\n\n")

  putStr ("Number of bases in each: " ++ (show $ map length non_trivial_mubs) ++ "\n")

