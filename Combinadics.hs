{-
  Reasonably fast enumerations for combinations, `recombinations', and
  permutations.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  A combination is a length m subset of {0 .. n - 1}.
  A `recombination' is a length m submultiset of {0 .. n - 1}.
  A permutation is a length m ordered subset of {0 .. n - 1}.

  General notes:
    - The num* functions determine how many of that particular object can be
      constructed.

    - The nat2* functions construct the unique * corresponding to the natural
      number provided.

    - The *2nat functions construct the unique natural number corresponding to
      the * provided.

    - select constructs a sublist of a provided list using indices from another
      provided list.

    - nat2* is slow, but next* is fast.  make* uses nat2* and next* to
      construct sequences of *s.

    - Unfortunately there is no nextPerm or makePerm (yet!).  For the time
      being, use Perms.hs when a large series of permutations is needed.
-}

module Combinadics (numCombs, comb2nat, nat2comb, nextComb, makeCombs,
                    numRecombs, recomb2nat, nat2recomb, nextRecomb, makeRecombs,
                    numPerms, perm2nat, nat2perm,
                    select) where


import Data.List
import Data.Maybe


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
  Length m combination in n variables corresponding to a natural number x.
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
  Number of length m recombinations in n variables.
-}
numRecombs :: Integer -> Integer -> Integer
numRecombs n m = n & m


{-
  Natural number corresponding to a recombination r in n variables.
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
  Length m recombination in n variables corresponding to a natural number x.
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
  Number of length m permutations.
-}
numPerms :: Integer -> Integer
numPerms m = fac m


{-
  Natural number corresponding to a factoradic f.
-}
fact2nat :: [Integer] -> Integer
fact2nat []      = 0
fact2nat (h : t) = h * (fac . toInteger $ length t) + (fact2nat t)


{-
  Length m factoradic corresponding to a natural number x.
-}
nat2fact :: Integer -> Integer -> [Integer]
nat2fact 0 _ = []
nat2fact m x = (div x (fac (m - 1))) : nat2fact (m - 1) (mod x (fac (m - 1)))


{-
  Natural number corresponding to a permutation p.
-}
perm2nat :: [Integer] -> Integer
perm2nat [] = 0
perm2nat p = fact2nat $ f' [0 .. (toInteger $ length p) - 1] p
             where f' _ []      = []
                   f' l (h : t) = h' : f' l' t
                                  where h' = (toInteger . fromJust) (findIndex (== h) l)
                                        l' = delete h l


{-
  Length m permutation corresponding to a natural number x.
-}
nat2perm :: Integer -> Integer -> [Integer]
nat2perm m x = f' [0 .. m - 1] (nat2fact m x)
               where f' _ []      = []
                     f' l (h : t) = h' : f' l' t
                                    where h' = genericIndex l h
                                          l' = delete h' l


{-
  Select elements from a list x based on indices in s.
-}
select :: [a] -> [Integer] -> [a]
select x s = map (genericIndex x) s


{-
  Test functions for combinations, recombinations, and permutations..  Should
  return a list of `True's.  Use `and' to test for failure.

  Note: these functions are not a good test of the speed of generation and
  do not test next*.
-}
testCombs :: Integer -> Integer -> [Bool]
testCombs n m = map (\ (a, b) -> a == b) $ zip xs x's
                where xs  = [0 .. (numCombs n m) - 1]
                      cs  = map (nat2comb n m) xs
                      x's = map (comb2nat n) cs

testRecombs :: Integer -> Integer -> [Bool]
testRecombs n m = map (\ (a, b) -> a == b) $ zip xs x's
                  where xs  = [0 .. (numRecombs n m) - 1]
                        rs  = map (nat2recomb n m) xs
                        x's = map (recomb2nat n) rs

testPerms :: Integer -> [Bool]
testPerms m = map (\ (a, b) -> a == b) $ zip xs x's
              where xs  = [0 .. (numPerms m) - 1]
                    ps  = map (nat2perm m) xs
                    x's = map (perm2nat) ps

