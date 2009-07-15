{-
  Mappings from vectors of dimension d (where each element is an integer 0 -> n - 1)
  integers and vice versa.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Magic (vec2magic, vecs2magics, magic2vec, magics2vecs) where

import Data.Set


{-
  Compute a vector's magic number.
-}
vec2magic (d, n) []        = 0
vec2magic (d, n) (xh : xt) = xh * n^(d - (length xt) - 2) + (vec2magic (d, n) xt)


{-
  Convert a set of vectors to a set of magic numbers.
-}
vecs2magics (d, n) s = Data.Set.map (vec2magic (d, n)) s


{-
  Compute a magic number's vector.
-}
magic2vec' (d, n) x y = mod (div x (n^y)) n
magic2vec  (d, n) x   = Prelude.map (magic2vec' (d, n) x) (take (d - 1) (iterate (1 +) 0))


{-
  Convert a set of magic numbers to a set of vectors.
-}
magics2vecs (d, n) s = Data.Set.map (magic2vec (d, n)) s

