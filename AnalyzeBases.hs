import Data.List
import Data.Binary
import Data.Complex
import Data.Array

{-
  Compute a magic number's vector.
-}
magic2vec' (d, n) x y = mod (div x (n^y)) n
magic2vec  (d, n) x   = map (magic2vec' (d, n) x) (range(0, d - 2))

{-
  Convert a base from a set of magic numbers to a set of vectors.
-}
magicbase2vecbase (d, n) x = map (magic2vec (d, n)) x


main = do
       -- print $ length $ nubBy pv_eq unit_ub
       -- print $ nubBy pv_eq unit_ub
       --Print all vectors which are unbiased to all ones and each other:
       -- print all_mutually_unbiased_lines
       -- print $ map length all_mutually_unbiased_lines
       --print non_trivial_mubs
       --print $ map length non_trivial_mubs
       f <- decodeFile "all_valid_hads.bin" :: IO [[Int]]
       print f
       --print (map (magicbase2vecbase (6, 12)) f)
       --print $ (length f)

