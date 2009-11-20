{-
  Print sets of MUBs as LaTeX source.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Magic


{-
  Convert a vector to a LaTeX string.
-}
vec2latex :: [Integer] -> String
vec2latex (vH : vT) = if   Prelude.null vT
                      then "$" ++ (show vH) ++ "$ \\\\\n"
                      else "$" ++ (show vH) ++ "$ & " ++ (vec2latex vT)


{-
  Convert a basis to a LaTeX string.
-}
basis2latex :: Integer -> [[Integer]] -> String
basis2latex d b =    "\\frac{1}{\\sqrt{" ++ (show d) ++ "}}\n\\left( \\begin{array}{"
                  ++ (foldl (++) "" $ genericReplicate d "c") ++ "}\n"
                  ++ (foldl (++) "" $ Prelude.map vec2latex b)
                  ++ "\\end{array} \\right)"


{-
  Convert a list of bases to a LaTeX string.
-}
list2latex :: Integer -> [[[Integer]]] -> String
list2latex d l          = "\\begin{equation}\n" ++ (list2latex' d l) ++ "\\end{equation}\n"
list2latex' d (lH : lT) = if   Prelude.null lT
                          then (basis2latex d lH) ++ "\n"
                          else (basis2latex d lH) ++ "\n,\n" ++ (list2latex' d lT)


{-
  MUBs2LaTeX <d> <n> <fMUBs>

  Dimension d.
  nth roots of unity.
  Sets of MUBs read from fMUBs, or standard input if fMUBs is "-".
-}
main = do
  {-
    Command line arguments.
  -}
  sD : (sN : (fMUBs : _)) <- getArgs
  let d = read sD :: Integer
  let n = read sN :: Integer


  {-
    Read a file of MUBs and convert all bases to matrices.
  -}
  mubs <- if  fMUBs == "-"
         then getContents
         else readFile fMUBs
  let mubs' = map (map ((map (0 :)) . (magics2vecs (d, n))) . read) (lines mubs) :: [[[[Integer]]]]

  {-
    Output LaTeX.
  -}
  let mubs'' = map (list2latex d) mubs'
  sequence_ $ map putStrLn mubs''

