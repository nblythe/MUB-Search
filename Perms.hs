{-
  Permutations

  2009 Nathan Blythe, Dr. Oscar Boykin

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
  Left-permute a list of lists by a specific permutation matrix.
-}
permuteL x p = map (x !!) (rowShuffle p)


{-
  Right-permute a list of lists by a specific permutation matrix.
-}
permuteR x p = transpose (permuteL (transpose x) p)


{-
  All right permutations of a list of lists.
-}
permuteAllR x = map (permuteR x) (allPerms (length x))


{-
  All left permutations of a list of lists.
-}
permuteAllL x = map (permuteL x) (allPerms (length x))


{-
  All permutations of a list of lists.
-}
permuteAll' [] = []
permuteAll' (xH : xT) = (permuteAllL xH) ++ (permuteAll' xT)
permuteAll x = permuteAll' (permuteAllR x)


{-
  Uniqueness of two lists of lists under some left and
  right permutations.
-}
permUnique x y = null (filter (== x) (permuteAll y))


{-
  Given a list of lists of lists l and a list of lists x, determine
  if the list of lists x is unique to all lists of lists in the list
  of lists of lists l (got that?).

  If it is unique, return the list with the unique element prepended.
  Otherwise just return the list.
-}
permUniqueToList l x = if   and (map (permUnique x) l)
                       then x : l
                       else l


{-
  Give a list of lists of lists, build a similar list with all
  equivalencies (under left and right permutations) removed.
-}
permUniqueList (xH : xT) = if   (null xT)
                           then [xH]
                           else permUniqueToList (permUniqueList xT) xH


