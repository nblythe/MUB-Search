{-
  Combinadics, factoradics, etc.

  2009 Nathan Blythe, Dr. Oscar Boykin

  A factoradic is a bijection between a subset of consecutive integers and the permutations of a set.
  A combinadic is a bijection between a subset of consecutive integers and the combinations of a set.
  A recombinadic is a combinadic on multisets (i.e. a combinadic with repetition/replacement).
-}

module Combinadics where

import Data.List


{-
  A handy factorial function.
-}
facs :: [Integer]
facs = scanl (*) 1 [1..]
fac :: Integer -> Integer
fac n = genericIndex facs n


{-
  A handy n-choose-k function.
-}
nchoosek :: Integer -> Integer -> Integer
nchoosek n k | k > n = 0
             | k == n = 1
             | otherwise = div (fac n) ((fac (n - k)) * (fac k))
(#) :: Integer -> Integer -> Integer
(#) n k = nchoosek n k
(&) :: Integer -> Integer -> Integer
(&) n k = nchoosek (n + k - 1) k


{-
  Number of length m combinadics in n variables.
-}
numCombinadics :: Integer -> Integer -> Integer
numCombinadics n m = n # m


{-
  Natural number corresponding to a combinadic r in n variables.
-}
combinadic2natural :: Integer -> [Integer] -> Integer
combinadic2natural n r = f (m - 1) r
                         where m                 = toInteger $ length r
                               f 0 _             = (n # m) - ((n - (head r)) # m)
                               f j (h1 : h2 : t) = ((n - h1 - 1) # j) - ((n - h2) # j) + f (j - 1) (h2 : t)


{-
  Length m combinadic in n variables corresponding to a natural number x.
-}
natural2combinadic :: Integer -> Integer -> Integer -> [Integer]
natural2combinadic n m x = f (-1) m x
                           where f _ 1 i = [i]
                                 f p d i = j : f j (d - 1) (i' j)
                                           where i' v = i - (n#d) + ((n-v)#d) + (n#(d-1)) - ((n-v-1)#(d-1))
                                                 j    = head $ filter (\ v ->    ((i' v) >= 0)
                                                                              && ((i' v) < ((n)#(d-1)))
                                                                              && (v > p))
                                                               [0 .. n - 1]


{-
  Number of length m recombinadics in n variables.
-}
numRecombinadics :: Integer -> Integer -> Integer
numRecombinadics n m = n & m


{-
  Natural number corresponding to a recombinadic r in n variables.
-}
recombinadic2natural :: Integer -> [Integer] -> Integer
recombinadic2natural n r = f (m - 1) r
                           where m                 = toInteger $ length r
                                 f 0 _             = (n & m) - ((n - (head r)) & m)
                                 f j (h1 : h2 : t) = ((n - h1) & j) - ((n - h2) & j) + f (j - 1) (h2 : t)


{-
  Length m recombinadic in n variables corresponding to a natural number x.
-}
natural2recombinadic :: Integer -> Integer -> Integer -> [Integer]
natural2recombinadic n m x = f 0 m x
                             where f _ 1 i = [i]
                                   f p d i = j : f j (d - 1) (i' j)
                                             where i' v = i - (n&d) + ((n-v)&d) + (n&(d-1)) - ((n-v)&(d-1))
                                                   j    = head $ filter (\ v ->    ((i' v) >= 0)
                                                                                && ((i' v) < (n&(d-1)))
                                                                                && (v >= p))
                                                                 [0 .. n - 1]


{-
  Select elements from a list x based on indices in s.
-}
select :: [a] -> [Integer] -> [a]
select x s = map (genericIndex x) s

