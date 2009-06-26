{-
  12th roots of unity.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Roots12 (roots, toInts) where

import Data.List
import Data.Maybe

import ExtRat
import ExtCpx

r0 = ExtRat 0   0 0 0             -- 0
r1 = ExtRat 0.5 0 0 0             -- 1/2
r2 = ExtRat 0   0 0.5 0           -- sqrt(3) / 2
r3 = ExtRat 1   0 0 0             -- 1

--                   Real   Imag     Root
roots = [ ExtCpx   r3     r0    -- 0
        , ExtCpx   r2     r1    -- 1
        , ExtCpx   r1     r2    -- 2
        , ExtCpx   r0     r3    -- 3
        , ExtCpx (-r1)    r2    -- 4
        , ExtCpx (-r2)    r1    -- 5
        , ExtCpx (-r3)    r0    -- 6
        , ExtCpx (-r2)  (-r1)   -- 7
        , ExtCpx (-r1)  (-r2)   -- 8
        , ExtCpx   r0   (-r3)   -- 9
        , ExtCpx   r1   (-r2)   -- 10
        , ExtCpx   r2   (-r1) ] -- 11


{-
  Convert a set of 12th roots of unity to a set of root numbers.
-}
toInts w = map (\x -> fromJust (findIndex (x ==) roots)) w

