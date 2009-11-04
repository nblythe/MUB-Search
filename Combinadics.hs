{-
  Combinadics, factoradics, etc.

  2009 Nathan Blythe, Dr. Oscar Boykin

  A combination is a length m subset {0 .. n - 1}
  A recombination is a length m multisubset of {0 .. n - 1}
  Here we provide bijective enumerations for combinations and recombinations.
-}

module Combinadics (numCombs, comb2nat, nat2comb, nextComb, makeCombs,
                    numRecombs, recomb2nat, nat2recomb, nextRecomb, makeRecombs,
                    select) where


{-
  Need the "genericIndex" and "genericTake" functions so we can index lists by
  Integers.
-}
import Data.List(genericIndex, genericTake)


{-
  A handy factorial function (memoized).
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


{-
  Infix notation for n-choose-k.
-}
(#) :: Integer -> Integer -> Integer
(#) n k = nchoosek n k


{-
  Infix notation for n-choose-k (with repetition/replacement).
-}
(&) :: Integer -> Integer -> Integer
(&) n k = nchoosek (n + k - 1) k


{-
  Number of length m combinations in n variables.
-}
numCombs :: Integer -> Integer -> Integer
numCombs n m = n # m


{-
  Natural number corresponding to a combination r in n variables.
-}
comb2nat :: Integer -> [Integer] -> Integer
comb2nat n r = f (m - 1) r
               where m                 = toInteger $ length r
                     f 0 _             =   (n # m)
                                         - ((n - (head r)) # m)
                     f j (h1 : h2 : t) =   ((n - h1 - 1) # j)
                                         - ((n - h2) # j)
                                         + f (j - 1) (h2 : t)


{-
  Length m combinadic in n variables corresponding to a natural number x.
-}
nat2comb :: Integer -> Integer -> Integer -> [Integer]
nat2comb n m x = f (-1) m x
                 where f _ 1 i = [i]
                       f p d i = j : f j (d - 1) (i' j)
                                 where i' v =   i
                                              - (n # d)
                                              + ((n - v) # d)
                                              + (n # (d - 1))
                                              - ((n - v - 1) # (d - 1))
                                       g v  =    ((i' v) >= 0)
                                              && ((i' v) < (n#(d-1)))
                                              && (v > p)
                                       j    = head $ filter g [0 .. n - 1]


{-
  Next combination in n variables.
-}
nextComb :: Integer -> [Integer] -> [Integer]
nextComb n x = snd $ f n x
               where f k (h : []) =   (h == k - 1, (h + 1)
                                    :  [] )
                     f k (h : t)  =   (h' == k - (toInteger $ length t), h'
                                    :  g h' r)
                                    where h'            = if   c
                                                          then h + 1
                                                          else h
                                          (c, r)        = f k t
                                          g v []        = []
                                          g v (lH : lT) = if   lH == k - (toInteger $ length lT)
                                                          then (v + 1) : (g (v + 1) lT)
                                                          else lH : lT


{-
  Length m combinations x through x + c - 1 (c combinations starting with x)
  in n variables.
-}
makeCombs :: Integer -> Integer -> Integer -> Integer -> [[Integer]]
makeCombs n m x c = genericTake c $ iterate (nextComb n) f
                    where f = nat2comb n m x


{-
  Number of length m recombinadics in n variables.
-}
numRecombs :: Integer -> Integer -> Integer
numRecombs n m = n & m


{-
  Natural number corresponding to a recombinadic r in n variables.
-}
recomb2nat :: Integer -> [Integer] -> Integer
recomb2nat n r = f (m - 1) r
                 where m                 = toInteger $ length r
                       f 0 _             =   (n & m)
                                           - ((n - (head r)) & m)
                       f j (h1 : h2 : t) =   ((n - h1) & j)
                                           - ((n - h2) & j)
                                           + f (j - 1) (h2 : t)


{-
  Length m recombinadic in n variables corresponding to a natural number x.
-}
nat2recomb :: Integer -> Integer -> Integer -> [Integer]
nat2recomb n m x = f 0 m x
                   where f _ 1 i = [i]
                         f p d i = j : f j (d - 1) (i' j)
                                   where i' v =   i
                                                - (n & d)
                                                + ((n - v) & d)
                                                + (n & (d - 1))
                                                - ((n - v) & (d - 1))
                                         g v  =    ((i' v) >= 0)
                                                && ((i' v) < (n&(d-1)))
                                                && (v >= p)
                                         j    = head $ filter g [0 .. n - 1]


{-
  Next recombination in n variables.
-}
nextRecomb :: Integer -> [Integer] -> [Integer]
nextRecomb n x = snd $ f n x
                 where f k (h : []) = (h == k - 1, (h + 1) : [])
                       f k (h : t)  = (h' == k, h' : g h' r)
                                      where h'            = if   c
                                                            then h + 1
                                                            else h
                                            (c, r)        = f k t
                                            g v []        = []
                                            g v (lH : lT) = if   lH == k
                                                            then v : (g v lT)
                                                            else lH : lT


{-
  Length m recombinations x through x + c - 1 (c recombinations starting with x)
  in n variables.
-}
makeRecombs :: Integer -> Integer -> Integer -> Integer -> [[Integer]]
makeRecombs n m x c = genericTake c $ iterate (nextRecomb n) f
                      where f = nat2recomb n m x


{-
  Select elements from a list x based on indices in s.
-}
select :: [a] -> [Integer] -> [a]
select x s = map (genericIndex x) s

