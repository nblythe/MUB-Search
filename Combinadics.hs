{-
  Combinadics, factoradics, etc.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Combinadics  where

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
nchoosek n k = div (fac n) ((fac (n - k)) * (fac k))
(#) :: Integer -> Integer -> Integer
(#) n k = nchoosek n k
(&) :: Integer -> Integer -> Integer
(&) n k = nchoosek (n + k - 1) k

{-
{-
  Natural number corresponding to a factoradic f.
-}
factoradic2nat :: [Integer] -> Integer
factoradic2nat (fH : fT) = fH * fac (length fT) + (factoradic2nat fT)


{-
  Length n factoradic corresponding to natural number x.
-}
nat2factoradic :: Int -> Integer -> [Int]
nat2factoradic 1 x = [fromInteger x, 0]
nat2factoradic n x = (div x (fac n)) : (nat2factoradic (n - 1) (mod x (fac n)))


{-
  Natural number corresponding to a recombinadic r in n variables.
-}
recombinadic2nat :: Int -> [Int] -> Integer
recombinadic2nat _ (_ : []) = 0
recombinadic2nat n (rH1 : rH2 : rT) = (f rH1) - (f rH2) + recombinadic2nat n (rH2 : rT)
                             where f v = nchoosek (n - v + (length rT)) (1 + length rT)


{-
  Length d recombinadic in n variables corresponding to natural number x.
-}
nat2recombinadic' :: Int -> Int -> Int -> Integer -> [Integer]
nat2recombinadic' _ _ 1 x = [x]
nat2recombinadic' n p d x = j : nat2recombinadic' j (d - 1) (i' j)
                            where i' v = x - ((nchoosek (n + d - 1) d) - (nchoosek (n - v + d - 1) d))
                                           + ((nchoosek (n + d - 2) (d - 1)) - (nchoosek (n - v + d - 2) (d - 1)))
                                  j = head $ filter (\ v -> ((i' v) >= 0) && (i' v) < (nchoosek (n + (d - 1) - 1) (d - 1)) && (v >= p)) [0 .. n - 1]

nat2recombinadic :: Int -> Int -> Integer -> [Integer]
nat2recombinadic n d x = nat2recombinadic' n 0 d x
-}


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

