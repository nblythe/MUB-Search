{-
  24th roots of unity, as extended complex rationals.

  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Roots24 (roots, toInts) where

import Data.List
import Data.Maybe

import Cyclotomic24


{-
  The 24th roots of unity.
-}
roots = [  Cyclotomic24   1       0       0       0       0       0       0       0
        ,  Cyclotomic24   0       0.25    0       0.25    0     (-0.25)   0       0.25
        ,  Cyclotomic24   0       0       0.50    0       0.50    0       0       0
        ,  Cyclotomic24   0       0.50    0       0       0       0.50    0       0
        ,  Cyclotomic24   0.50    0       0       0       0       0       0.50    0
        ,  Cyclotomic24   0     (-0.25)   0       0.25    0       0.25    0       0.25
        ,  Cyclotomic24   0       0       0       0       1       0       0       0
        ,  Cyclotomic24   0     ( 0.25)   0     (-0.25)   0       0.25    0       0.25
        ,  Cyclotomic24 (-0.50)   0       0       0       0       0       0.50    0
        ,  Cyclotomic24   0     (-0.50)   0       0       0       0.50    0       0
        ,  Cyclotomic24   0       0     (-0.50)   0       0.50    0       0       0
        ,  Cyclotomic24   0     (-0.25)   0     (-0.25)   0     (-0.25)   0       0.25
        ,  Cyclotomic24 (-1   )   0       0       0       0       0       0       0
        ,  Cyclotomic24   0     (-0.25)   0     (-0.25)   0       0.25    0     (-0.25)
        ,  Cyclotomic24   0       0     (-0.50)   0     (-0.50)   0       0       0
        ,  Cyclotomic24   0     (-0.50)   0       0       0     (-0.50)   0       0
        ,  Cyclotomic24 (-0.50)   0       0       0       0       0     (-0.50)   0
        ,  Cyclotomic24   0       0.25    0     (-0.25)   0     (-0.25)   0     (-0.25)
        ,  Cyclotomic24   0       0       0       0     (-1)      0       0       0
        ,  Cyclotomic24   0     (-0.25)   0       0.25    0     (-0.25)   0     (-0.25)
        ,  Cyclotomic24   0.50    0       0       0       0       0     (-0.50)   0 
        ,  Cyclotomic24   0       0.50    0       0       0     (-0.50)   0       0
        ,  Cyclotomic24   0       0       0.50    0     (-0.50)   0       0       0
        ,  Cyclotomic24   0       0.25    0       0.25    0       0.25    0     (-0.25) ]


{-
  Convert a set of 24th roots of unity to a set of root numbers.
-}
toInts w = map (\x -> fromJust (findIndex (x ==) roots)) w

