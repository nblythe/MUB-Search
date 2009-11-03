{-
  Combinadics, factoradics, etc.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Combinadics (numCombinations, combination2natural, natural2combination, nextCombination,
                    numRecombinations, recombination2natural, natural2recombination, nextRecombination,
                    select) where

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
  Number of length m combinations in n variables.
-}
numCombinations :: Integer -> Integer -> Integer
numCombinations n m = n # m


{-
  Natural number corresponding to a combination r in n variables.
-}
combination2natural :: Integer -> [Integer] -> Integer
combination2natural n r = f (m - 1) r
                          where m                 = toInteger $ length r
                                f 0 _             = (n # m) - ((n - (head r)) # m)
                                f j (h1 : h2 : t) = ((n - h1 - 1) # j) - ((n - h2) # j) + f (j - 1) (h2 : t)


{-
  Length m combinadic in n variables corresponding to a natural number x.
-}
natural2combination :: Integer -> Integer -> Integer -> [Integer]
natural2combination n m x = f (-1) m x
                            where f _ 1 i = [i]
                                  f p d i = j : f j (d - 1) (i' j)
                                            where i' v = i - (n#d) + ((n-v)#d) + (n#(d-1)) - ((n-v-1)#(d-1))
                                                  j    = head $ filter (\ v ->    ((i' v) >= 0)
                                                                               && ((i' v) < ((n)#(d-1)))
                                                                               && (v > p))
                                                                [0 .. n - 1]


{-
  Next combination in n variables.
-}
nextCombination :: Integer -> [Integer] -> [Integer]
nextCombination n x = snd $ f n x
                      where f k (h : []) = (h == k - 1, (h + 1) : [])
                            f k (h : t)  = (h' == k - (toInteger $ length t), h' : g h' r)
                                           where h' = if   c
                                                      then h + 1
                                                      else h
                                                 (c, r) = f k t
                                                 g v []        = []
                                                 g v (lH : lT) = if   lH == k - (toInteger $ length lT)
                                                                 then (v + 1) : (g (v + 1) lT)
                                                                 else lH : lT


{-
  Number of length m recombinadics in n variables.
-}
numRecombinations :: Integer -> Integer -> Integer
numRecombinations n m = n & m


{-
  Natural number corresponding to a recombinadic r in n variables.
-}
recombination2natural :: Integer -> [Integer] -> Integer
recombination2natural n r = f (m - 1) r
                            where m                 = toInteger $ length r
                                  f 0 _             = (n & m) - ((n - (head r)) & m)
                                  f j (h1 : h2 : t) = ((n - h1) & j) - ((n - h2) & j) + f (j - 1) (h2 : t)


{-
  Length m recombinadic in n variables corresponding to a natural number x.
-}
natural2recombination :: Integer -> Integer -> Integer -> [Integer]
natural2recombination n m x = f 0 m x
                              where f _ 1 i = [i]
                                    f p d i = j : f j (d - 1) (i' j)
                                              where i' v = i - (n&d) + ((n-v)&d) + (n&(d-1)) - ((n-v)&(d-1))
                                                    j    = head $ filter (\ v ->    ((i' v) >= 0)
                                                                                 && ((i' v) < (n&(d-1)))
                                                                                 && (v >= p))
                                                                  [0 .. n - 1]


{-
  Next recombination in n variables.
-}
nextRecombination :: Integer -> [Integer] -> [Integer]
nextRecombination n x = snd $ f n x
                        where f k (h : []) = (h == k - 1, (h + 1) : [])
                              f k (h : t)  = (h' == k, h' : g h' r)
                                             where h' = if   c
                                                        then h + 1
                                                        else h
                                                   (c, r) = f k t
                                                   g v []        = []
                                                   g v (lH : lT) = if   lH == k
                                                                   then v : (g v lT)
                                                                   else lH : lT


{-
  Select elements from a list x based on indices in s.
-}
select :: [a] -> [Integer] -> [a]
select x s = map (genericIndex x) s

