{-
  Permutations

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)

  Functions for constructing permutations of lists of lists and comparing
  lists of lists for equivalence under permutations.
-}

module Perms where

import Data.List
import Maybe
import Ix


{-
  Construct rows of an n x n permutation matrix with 1s
  in locations indicated by x.

  Ex. makePermRows 3 [0, 2]
      [[1, 0, 0], [0,0,1]]
-}
makePermRows n x = map (\y -> (take y (repeat 0))
                            ++ [1]
                            ++ (take (n - y - 1) (repeat 0))
                       ) x


{-
  Construct all next possible permutation rows of a partial
  n x n permutation matrix.

  Ex. nextPermRows 3 [[1, 0, 0]]
      [[0,1,0], [0,0,1]]
-}
nextPermRows :: Int -> [[Int]] -> [[Int]]
nextPermRows n p = if   (null p)
                  then  makePermRows n (range(0, n - 1))
                  else  makePermRows n (foldl (intersect) (range(0, n - 1)) (map (findIndices (== 0)) p))


{-
  All partial n x n permutation matrices that can be constructed
  from the partial n x n permutation matrices in l.

  Ex. nextPerms 3 [[[1, 0, 0]], [[0, 1, 0]]]
      [[[1,0,0],[0,1,0]], [[1,0,0],[0,0,1]],
       [[0,1,0],[1,0,0]], [[0,1,0],[0,0,1]]
      ]
-}
nextPerms n l = foldl (union)
                      []
                      (map (\y -> map (\x -> y ++ [x])
                                      (nextPermRows n y))
                           l)


{-
  All n x n permutation matrices.
-}
allPerms n = (iterate (nextPerms n) [[]]) !! n


{-
  Convert a permutation matrix to a list of indices, by row
  (a row shuffle).
-}
rowShuffle p = map (\x -> head (findIndices (== 1) x)) p


{-
  Permute a list of lists by a specific permutation matrix.
-}
permuteL x p = map (x !!) (rowShuffle p)
permuteR x p = transpose (permuteL (transpose x) p)


{-
  All permutations of a list of lists.
-}
permuteAllL x = map (permuteL x) (allPerms (length x))
permuteAllR x = map (permuteR x) (allPerms (length x))
permuteAll x = concat $ Prelude.map permuteAllR (permuteAllL x)


{-
  Uniqueness of two lists of lists under some permutation.
-}
permUniqueL x y = all (/= y) (permuteAllL x)
permUniqueR x y = all (/= y) (permuteAllR x)
permUnique x y = all (/= y) (permuteAll x)


{-
  Extend a list l with a list of lists x, if x is unique to all
  lists of lists in l.
-}
permUniqueToListL l x = if   all (permUniqueL x) l
                        then x : l
                        else l
permUniqueToListR l x = if   all (permUniqueR x) l
                        then x : l
                        else l
permUniqueToList l x = if   all (permUnique x) l
                       then x : l
                       else l


{-
  Give a list of lists of lists, build a similar list with all
  equivalencies removed.
-}
permUniqueListL (xH : xT) = if   (null xT)
                            then [xH]
                            else permUniqueToListL (permUniqueListL xT) xH
permUniqueListR (xH : xT) = if   (null xT)
                            then [xH]
                            else permUniqueToListR (permUniqueListR xT) xH
permUniqueList (xH : xT) = if   (null xT)
                           then [xH]
                           else permUniqueToList (permUniqueList xT) xH

