import Data.List
import Data.Binary
import Data.Complex
import Data.Array

import Magic


{- what dimension are we working in -}
dim :: Int
dim = 6
nth_roots :: Int
nth_roots = 12
tiny :: Float
tiny = 0.0001

type HadV = [Int] -- Hadamard Vector
type HadNode = [HadV] -- Hadamard Node, or part of a Hadamard Matrix, some number of columns





{-
  Add two dimension dim vectors mod nth_roots and find the magic number of
  the resulting vector.
-}
addVec2magic a b = vec2magic (dim, nth_roots) (zipWith (\x y -> mod (x + y) nth_roots) a b)


{-
  Determine if two vectors are listed in a table.
-}
are_inTable tab a b = not (Nothing == (find ((addVec2magic a b) ==) tab))










opmod :: Integral a => (a -> a -> a) -> a -> a -> a -> a
opmod f m x y = mod ( f x y ) m 
pmod = opmod (+)
mmod = opmod (-)


-- Return true if these two vectors are unbiased.  We assume the first element
-- is 1, the inner product should be \sqrt{d}
are_unbiased :: [Int] -> [Int] -> HadV -> HadV -> Bool
are_unbiased orthT biasT xs ys = are_inTable biasT xs ys

-- Return true if two matrices are unbiased:
are_unbiased_had :: [Int] -> [Int] -> HadNode -> HadNode -> Bool
are_unbiased_had orthT biasT h1 h2 = and [ are_unbiased orthT biasT c1 c2 | c1 <- h1, c2 <- h2 ]

-- Treat a vector like a diagonal matrix and multiply by a Hadamard
d_mult :: HadV -> HadNode -> HadNode
d_mult v h = [ [mod (vi+wi) nth_roots | (vi, wi) <- zip v w] | w <- h]

{- The biggest possible vector -}
max_v :: HadV
max_v = take (dim-1) (repeat (nth_roots - 1))
{- The smallest possible vector -}
min_v :: HadV
min_v = take (dim-1) (repeat 0)
{- Is this the largest vector in our ordering -}
is_max :: HadV -> Bool
is_max = (== max_v)
l_max = length max_v

gt_e_max :: HadV -> Bool
gt_e_max v = (is_max v) || ((length v) > l_max)

{- Here's the next function 
 - This is just "little-endian" addition
 - -}
next_v :: HadV -> HadV 
next_v [] = [1]
next_v (x:xs) = if x < nth_roots-1
                then (x+1):xs
                else 0 : next_v xs
{- Convert a HadV to a integer
 -}
had_v_to_int :: HadV -> Int
had_v_to_int [] = 0
had_v_to_int (x:xs) = x + nth_roots * had_v_to_int xs

-- A list of all vectors from the minimum to the maximum (inclusive)
all_vecs :: [HadV]
all_vecs = all_vecs' min_v
           where all_vecs' v | not $ is_max v = v : all_vecs' (next_v v)
                             | otherwise = v : []




are_orth_a orthT biasT xs ys = are_inTable orthT xs ys

{- Make all the children of a given hadamard -}

get_children_n :: (HadV -> HadV) -> HadNode -> [ HadNode ]
get_children_n next_f hn | length hn == dim = []
                | otherwise = g_c' hn [] $ head hn 
                where g_c' hn acc v | gt_e_max v = (v:hn):acc
                                    | otherwise = g_c' hn ((v:hn):acc) (next_f v)
-- Get's all the children.
get_children = get_children_n next_v

{- If the most recently added vector is orthogonal to all previous, it's okay
 - since we check after adding each vector
 -}
is_valid :: [Int] -> [Int] -> HadNode -> Bool
-- is_valid (v:vs) = and [ are_orth v z | z <- vs]
-- is_valid (v:vs) = and [ are_orth_l v z | z <- vs]
is_valid orthT biasT (v:vs) = and [ are_orth_a orthT biasT v z | z <- vs]

make_valid_children :: [Int] -> [Int] -> HadNode -> [ HadNode ]
make_valid_children orthT biasT h = filter (is_valid orthT biasT) $ get_children h
-- make_valid_children h = filter is_valid $ get_children_n (next_v . next_v) h

make_valid_leafs :: [Int] -> [Int] -> HadNode -> [ HadNode ]
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
unit_ub :: [Int] -> [Int] -> [HadV]
unit_ub orthT biasT = filter (are_unbiased orthT biasT min_v)  all_vecs

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
all_mutually_unbiased_lines :: [Int] -> [Int] -> [[HadV]]
all_mutually_unbiased_lines orthT biasT = [ constrained_add (are_unbiased orthT biasT) [v] (unit_ub orthT biasT)  | v <- (unit_ub orthT biasT) ]

all_valid_hads orthT biasT = remove_equivs ( make_valid_leafs orthT biasT [ min_v ] )

-- A list of sets of mutually unbiased hads made from multiplying unbiased
-- lines with hadamard matrices
mubs_from_lines :: [Int] -> [Int] -> [[HadNode]]
mubs_from_lines orthT biasT = [ constrained_add (are_unbiased_had orthT biasT) [h]
                                [ d_mult diag h2 | diag <- diags, h2 <- (all_valid_hads orthT biasT) ]
                              | diags <- (all_mutually_unbiased_lines orthT biasT),
                              h <- (all_valid_hads orthT biasT) ]

non_trivial_mubs orthT biasT = filter ((> 1) . length) (mubs_from_lines orthT biasT)

main = do
  orth12 <- decodeFile "orth12.bin" :: IO [Int]
  unbias12 <- decodeFile "unbias12.bin" :: IO [Int]

       -- print $ length $ nubBy pv_eq unit_ub
       -- print $ nubBy pv_eq unit_ub
       --Print all vectors which are unbiased to all ones and each other:
       -- print all_mutually_unbiased_lines
       -- print $ map length all_mutually_unbiased_lines
       --print non_trivial_mubs
  print $ map length (non_trivial_mubs orth12 unbias12)
       --encodeFile "all_valid_hads.bin" all_valid_hads
       --encodeFile "non_trivial_mubs.bin" non_trivial_mubs
       --encodeFile "mubs_from_lines.bin" mubs_from_lines
       --encodeFile "all_mutually_unbiased_lines" all_mutually_unbiased_lines
       --print $ filter ((> 5) . length ) all_mutually_unbiased_lines
       -- print $ length unit_ub
       -- print $ remove_equivs $ make_valid_leafs [ min_v ]
       --print $ are_equiv (dim-1) (x!!0) (x!!1)
       --print $ all_perm_had (dim-1) (x!!1)
{-
main = let x = [1,2]
           y = [2,1]
           in do print (are_orth x y)
                 print (is_max [2,1])
                 print (next_v [1,2])
                 print (next_v [2,2])
-}
