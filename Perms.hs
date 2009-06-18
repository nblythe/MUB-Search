{-
  Permutation matrices
  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module Perms where

import Data.List
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
  from the partial n x n permutation matricse in l.

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
  All right and left permutations of a list of lists.
-}
permuteAll x = union (map (permuteL x) (allPerms (length x)))
                     (map (permuteR x) (allPerms (length x)))

{-
  Uniqueness of two lists of lists under some left and
  right permutations.
-}
permUnique x y = null (filter (== x) (permuteAll y))


