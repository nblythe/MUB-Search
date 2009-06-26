{-
  Mappings from vectors of dimension d (where each element is a value
  0 - n) to integers, and vice versa.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Magic (vec2magic, magic2vec) where


{-
  Compute a vector's magic number.
-}
vec2magic (d, n) []        = 0
vec2magic (d, n) (xh : xt) = xh * n^(d - (length xt) - 2) + (vec2magic (d, n) xt)


{-
  Compute a magic number's vector.
-}
magic2vec' (d, n) x y = mod (div x (n^y)) n
magic2vec  (d, n) x   = map (magic2vec' (d, n) x) (take (d - 2) (iterate (1 +) 0))

