{-
  24th roots of unity, as extended complex rationals.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Roots24 (roots, toInts) where

import Data.List
import Data.Maybe

import ExtRat
import ExtCpx

r0 = ExtRat 0    0       0    0       -- 0
r1 = ExtRat 0    (-0.25) 0    0.25    -- 0.25882
r2 = ExtRat 0.50 0       0    0       -- 0.5
r3 = ExtRat 0    0.50    0    0       -- 0.70711
r4 = ExtRat 0    0       0.50 0       -- 0.86603
r5 = ExtRat 0    0.25    0    0.25    -- 0.96593
r6 = ExtRat 1    0       0    0       -- 1

--                   Real   Imag       Root
roots = [  ExtCpx   r6        r0    -- 0
        ,  ExtCpx   r5        r1    -- 1
        ,  ExtCpx   r4        r2    -- 2
        ,  ExtCpx   r3        r3    -- 3
        ,  ExtCpx   r2        r4    -- 4
        ,  ExtCpx   r1        r5    -- 5
        ,  ExtCpx   r0        r6    -- 6
        ,  ExtCpx (-r1)       r5    -- 7
        ,  ExtCpx (-r2)       r4    -- 8
        ,  ExtCpx (-r3)       r3    -- 9
        ,  ExtCpx (-r4)       r2    -- 10
        ,  ExtCpx (-r5)       r1    -- 11
        ,  ExtCpx (-r6)       r0    -- 12
        ,  ExtCpx (-r5)     (-r1)   -- 13
        ,  ExtCpx (-r4)     (-r2)   -- 14
        ,  ExtCpx (-r3)     (-r3)   -- 15
        ,  ExtCpx (-r2)     (-r4)   -- 16
        ,  ExtCpx (-r1)     (-r5)   -- 17
        ,  ExtCpx   r0      (-r6)   -- 18
        ,  ExtCpx   r1      (-r5)   -- 19
        ,  ExtCpx   r2      (-r4)   -- 20
        ,  ExtCpx   r3      (-r3)   -- 21
        ,  ExtCpx   r4      (-r2)   -- 22
        ,  ExtCpx   r5      (-r1) ] -- 23


{-
  Convert a set of 24th roots of unity to a set of root numbers.
-}
toInts w = map (\x -> fromJust (findIndex (x ==) roots)) w

