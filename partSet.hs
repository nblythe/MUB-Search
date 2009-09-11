import System(getArgs)
import Data.Set
import Data.Binary
import Ratio
import Complex






{-
  A list of elements split into sublists of k elements each (and one possibly smaller).
-}
splitList k l = if    Prelude.null l
                then  []
                else  [l !! x | x <- [0.. n - 1]] : (splitList k $ drop n l)
                where n = if   (length l) >= k
                          then k
                          else length l


{-
  Given a list l, construct the list of all sets of m elements from l.
-}
--choose l m = if   m == 1
--             then [singleton x | x <- l]
--             else [Data.Set.union x (singleton y) | x <- choose l (m - 1), y <- Prelude.filter (\ z -> not $ member z x) l]


{-
  Consider a list l of m elements.  Construct all sublists of k elements from l.  Instead
  of constructing the actual sublists, we construct lists of indices into l.
-}
klists m k = if   k == 1
             then [[x] | x <- [0 .. m - 1]]
             else [x : y | x <- [0 .. m - 1], y <- klists m (k - 1)]


{-
  Select elements from a list l based on indices in a list u, forming a set.
-}
select l u = fromList $ Prelude.map (l !!) u


{-
  Partition a set s into subsets, each containing n sets of k elements from s.
-}
partSet s n k = Prelude.map (select l) (klists (length l) n)
                where l = (splitList k $ elems s)


{-
  Partition a set s into subsets of size k and then construct the set of all sets of m such subsets.
-}
--partChooseSet s k m = fromList $ choose ls m
--                      where ll = partitionList k (elems s)
--                            ls = Prelude.map fromList ll





main = do
  c : (g : (fName : argsT)) <- getArgs

  s <- decodeFile fName :: IO (Set Int)
  putStr ("Read " ++ (show $ size s) ++ " elements.\n")

  let s' = splitList (read g) $ elems s
  putStr ("Split into " ++ (show $ length s') ++ " groups of " ++ g ++ " elements each.\n")

  let i = klists (length s') (read c)
  let j = Prelude.map (select s') i
  let n = zip [fName ++ (show k) | k <- [0 .. (length j) - 1]] j
  putStr ("Constructed all lists of " ++ c ++ " such groups (total: " ++ (show $ length j) ++ " lists).\n")

  putStr ("Writing to " ++ fName ++ ".0, " ++ fName ++ ".1, ... " ++ fName ++ "." ++ (show $ (length j) - 1) ++ ".\n")
  let ops = Prelude.map (\ (f, d) -> encodeFile f d) n
  sequence ops

  putStr ("Done.\n")

