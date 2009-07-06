import Data.List
import Data.Binary
import Data.Complex
import Data.Array

import Magic
import VecTables


{- what dimension are we working in -}
dim :: Int
dim = 6
nth_roots :: Int
nth_roots = 12

type HadV = [Int] -- Hadamard Vector
type HadNode = [HadV] -- Hadamard Node, or part of a Hadamard Matrix, some number of columns



{-
  Floating point method.
-}
tiny :: Float
tiny = 0.0001

{- Converts k -> exp(2\pi i k/nth_roots) -}
int_to_comp :: Int -> Complex Float
int_to_comp k = cis $ 2.0 * pi * (fromIntegral k) / (fromIntegral nth_roots)

{-- Compute this once --}
int_to_comp_a = listArray (-nth_roots, nth_roots) [ int_to_comp k | k <- range (-nth_roots, nth_roots)]

opmod :: Integral a => (a -> a -> a) -> a -> a -> a -> a
opmod f m x y = mod ( f x y ) m 
pmod = opmod (+)
mmod = opmod (-)

--inner product of the two vectors
ip :: HadV -> HadV -> Complex Float
ip xs ys = (sum $ map (int_to_comp_a !) [ x - y | (x,y) <- zip xs ys ]) + 1

{- Assuming that the HadV has i at position k meaning
 - k is nth_roots of unity to the ith power
 -}
are_orth_fp orthT biasT xs ys = (magnitude (ip xs ys)) < tiny

-- Return true if these two vectors are unbiased.  We assume the first element
-- is 1, the inner product should be \sqrt{d}
are_unbiased_fp orthT biasT xs ys = abs( (magnitude (ip xs ys)) - sqrt (fromIntegral dim) ) < tiny






{-
  Pre-computed table method.
-}


{-
  Convert a sparse boolean table to a dense boolean table of length n.
-}
sparse2dense n tab = [not (Nothing == (find (x ==) tab)) | x <- take n (iterate (1 +) 0)]


{-
  Add two dimension dim vectors mod nth_roots and find the magic number of
  the resulting vector.
-}
addVec2magic a b = vec2magic (dim, nth_roots) (zipWith (\x y -> mod (x + y) nth_roots) a b)


{-
  Determine if two vectors are orthogonal.
-}
are_orth_pt orthT biasT u v = orthT ! (addVec2magic u v)
--are_orth orthT biasT u v = (sparse2dense (nth_roots^(dim - 1)) vecTableOrth12) !! (addVec2magic u v)


{-
  Determine if two vectors are unbiased.
-}
are_unbiased_pt orthT biasT u v = biasT ! (addVec2magic u v)






{-
  Method selection.
-}
are_orth = are_orth_fp
are_unbiased = are_unbiased_fp

{-
  Determine if two matrices are unbiased.
-}
are_unbiased_had orthT biasT a b = and [are_unbiased orthT biasT u v | u <- a, v <- b]


{-
  Treat a vector like a diagonal matrix and multiply by a Hadamard
-}
d_mult v h = [ [mod (vi+wi) nth_roots | (vi, wi) <- zip v w] | w <- h]


{-
  The largest vector.
-}
maxVec = replicate (dim - 1) (nth_roots - 1)


{-
  The smallest vector.
-}
minVec = replicate (dim - 1) 0


{-
  All vectors.
-}
nextVec []        = [1]
nextVec (xH : xT) = if   xH < nth_roots - 1
                           then (xH + 1) : xT
                           else 0 : nextVec xT
allVecs = take (nth_roots^(dim - 1)) (iterate nextVec minVec)





{- Is this the largest vector in our ordering -}
is_max :: HadV -> Bool
is_max = (== maxVec)
l_max = length maxVec

gt_e_max :: HadV -> Bool
gt_e_max v = (is_max v) || ((length v) > l_max)


{- Convert a HadV to a integer
 -}
had_v_to_int :: HadV -> Int
had_v_to_int [] = 0
had_v_to_int (x:xs) = x + nth_roots * had_v_to_int xs






{- Make all the children of a given hadamard -}

get_children_n :: (HadV -> HadV) -> HadNode -> [ HadNode ]
get_children_n next_f hn | length hn == dim = []
                | otherwise = g_c' hn [] $ head hn 
                where g_c' hn acc v | gt_e_max v = (v:hn):acc
                                    | otherwise = g_c' hn ((v:hn):acc) (next_f v)
-- Get's all the children.
get_children = get_children_n nextVec

{- If the most recently added vector is orthogonal to all previous, it's okay
 - since we check after adding each vector
 -}
is_valid orthT biasT (v:vs) = and [ are_orth orthT biasT v z | z <- vs]

make_valid_children orthT biasT h = filter (is_valid orthT biasT) $ get_children h

make_valid_leafs orthT biasT h = let ch = make_valid_children orthT biasT h
                                 in if null ch
                                    then if length h == dim
                                         then [ h ]
                                         else []
                                    else concatMap (make_valid_leafs orthT biasT) ch

-- Here are some permutation functions to remove equivalent hadaramards:
fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n-1)

perm_list :: Int -> Int -> [Int]
perm_list 0 _ = []
perm_list elems n = let (d,m) = divMod n elems
                    in m:perm_list (elems - 1) d

new_idx :: [Int] -> Int -> Int
new_idx [] idx = idx
new_idx (y:ys) idx = if idx >= y then new_idx ys (idx+1)
                     else new_idx ys idx

pl_to_p' :: [Int] -> [Int] -> [Int]
pl_to_p' first [] = first
pl_to_p' first (x:xs) = let t_idx = new_idx (sort first) x
                            f' = first ++ [t_idx]
                        in pl_to_p' f' xs
pl_to_p = pl_to_p' []
perm :: Int -> Int -> [Int]
perm n k = pl_to_p $ perm_list n k
-- Permute a vector
perm_v :: Int -> Int -> [Int] -> [Int]
perm_v n k v = [ v!!x | x <- perm n k]

-- Check if two hadamards are equivalent under left and right permutations
perm_h n had k = map (perm_v n k) had
all_perm_had n had = map (perm_h n had) (range (0,(fac n)-1))

-- A list of all permutations of a given vector
all_perm_v :: Int -> HadV -> [HadV]
all_perm_v n v = map (flip (perm_v n) v) (range (0,(fac n)-1))

-- Here are all the vectors unbiased to the all unity vector:
unit_ub orthT biasT = filter (are_unbiased orthT biasT minVec)  allVecs

pv_eq x y = elem x (all_perm_v (dim-1) y)

same_h had1 had2 = null (had1 \\ had2)

are_equiv n had1 had2 = let allh1 = all_perm_had n had1
                        in or $ map (same_h had2) allh1
remove_equivs' :: [HadNode] -> [HadNode] -> [HadNode]
remove_equivs' hs [] = hs
remove_equivs' [] (h:hs) = remove_equivs' [h] hs
remove_equivs' h0s (h:hs) = if or $ map (are_equiv (dim-1) h) h0s -- Already in the list
                            then remove_equivs' h0s hs
                            else remove_equivs' (h:h0s) hs -- Found a new one
remove_equivs hs = remove_equivs' [] hs
--remove_equivs = nubBy (are_equiv (dim-1))

-- For some symmetric boolean function (f x y == f y x), add each item so that
-- the function is true between all pairs:
constrained_add :: (a -> a -> Bool) -> [a] -> [a] -> [a]
constrained_add f x [] = x
constrained_add f [] (x:xs) = constrained_add f [x] xs
constrained_add f to (x:xs) = if and (map (f x) to) then constrained_add f (x:to) xs 
                              else constrained_add f to xs

-- All sets unbiased to the all 1 vector
all_mutually_unbiased_lines orthT biasT = [ constrained_add (are_unbiased orthT biasT) [v] (unit_ub orthT biasT)  | v <- (unit_ub orthT biasT) ]
all_valid_hads orthT biasT = remove_equivs ( make_valid_leafs orthT biasT [ minVec ] )

-- A list of sets of mutually unbiased hads made from multiplying unbiased
-- lines with hadamard matrices
mubs_from_lines orthT biasT = [ constrained_add (are_unbiased_had orthT biasT) [h]
                                [ d_mult diag h2 | diag <- diags, h2 <- (all_valid_hads orthT biasT) ]
                              | diags <- (all_mutually_unbiased_lines orthT biasT),
                              h <- (all_valid_hads orthT biasT) ]

non_trivial_mubs orthT biasT = filter ((> 1) . length) (mubs_from_lines orthT biasT)

main = do
  orth12S <- decodeFile "orth12.bin" :: IO [Int]
  unbias12S <- decodeFile "unbias12.bin" :: IO [Int]

  let orth12D = sparse2dense (length allVecs) orth12S
  let unbias12D = sparse2dense (length allVecs) unbias12S

  let orth12A = array (0, length orth12D) (zip (range (0, length orth12D)) orth12D)
  let unbias12A = array (0, length unbias12D) (zip (range (0, length unbias12D)) unbias12D)

       -- print $ length $ nubBy pv_eq unit_ub
       -- print $ nubBy pv_eq unit_ub
       --Print all vectors which are unbiased to all ones and each other:
       -- print all_mutually_unbiased_lines
       -- print $ map length all_mutually_unbiased_lines
       --print non_trivial_mubs
  print $ map length (non_trivial_mubs orth12A unbias12A)

