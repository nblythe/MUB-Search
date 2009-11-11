{-
  Print sets of MUBs as LaTeX source.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  TODO: needs updating for list-bases bases.
-}

{-
import Data.Binary
import Data.Set
import Data.List
import System(getArgs)

import Magic

{-
  Convert a vector to a LaTeX string.
-}
vec2latex (vH : vT) = if   Prelude.null vT
                      then "$" ++ (show vH) ++ "$ \\\\\n"
                      else "$" ++ (show vH) ++ "$ & " ++ (vec2latex vT)


{-
  Convert a basis to a LaTeX string.
-}
basis2latex d b =    "\\frac{1}{\\sqrt{" ++ (show d) ++ "}}\n\\left( \\begin{array}{"
                  ++ (foldl (++) "" $ replicate d "c") ++ "}\n"
                  ++ (foldl (++) "" $ Prelude.map vec2latex b)
                  ++ "\\end{array} \\right)"


{-
  Convert a list of bases to a LaTeX string.
-}
list2latex d l          = "\\begin{equation}\n" ++ (list2latex' d l) ++ "\\end{equation}\n"
list2latex' d (lH : lT) = if   Prelude.null lT
                          then (basis2latex d lH) ++ "\n"
                          else (basis2latex d lH) ++ "\n,\n" ++ (list2latex' d lT)

-}

{-
  MUBs2LaTeX <MUBs file>
-}
main = do
{-  argMUBs : argT <- getArgs

  mubs_d <- decodeFile argMUBs :: IO [(Set (Set Int))]

  let mubs_i = Prelude.map (elems . Data.Set.map (elems . Data.Set.map ((0 :) . magic2vec (6, 12)))) mubs_d

  let mubs_s = Prelude.map (list2latex 6) mubs_i
  putStr $ foldl (++) "" mubs_s
-}
  print "Update me!"

