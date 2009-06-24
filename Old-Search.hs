import Data.List
import Data.Binary
import Data.Complex
import Data.Array

{- what dimension are we working in -}
dim :: Int
dim = 6
nth_roots :: Int
nth_roots = 12
tiny :: Float
tiny = 0.0001

type HadV = [Int] -- Hadamard Vector
type HadNode = [HadV] -- Hadamard Node, or part of a Hadamard Matrix, some number of columns

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
are_orth :: HadV -> HadV -> Bool
are_orth xs ys = (magnitude (ip xs ys)) < tiny

-- Return true if these two vectors are unbiased.  We assume the first element
-- is 1, the inner product should be \sqrt{d}
are_unbiased :: HadV -> HadV -> Bool
are_unbiased xs ys = abs( (magnitude (ip xs ys)) - sqrt (fromIntegral dim) ) < tiny

-- Return true if two matrices are unbiased:
are_unbiased_had :: HadNode -> HadNode -> Bool
are_unbiased_had h1 h2 = and [ are_unbiased c1 c2 | c1 <- h1, c2 <- h2 ]

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
-- Here's a list of all vectors unbiased to all 1:
orth_to_one :: [HadV]
orth_to_one = map sort $ nubBy pv_eq (filter (are_orth min_v) all_vecs)



-- Here's a list based are_orth function, which may be faster:
are_orth_l xs ys = let z = sort [ mod (x-y) nth_roots | (x,y) <- zip xs ys]
                   in elem z orth_to_one 

{-- Compute this once --}
orth_to_one_a = listArray (had_v_to_int min_v, had_v_to_int max_v)
                          [ are_orth min_v x | x <- all_vecs ]
are_orth_a xs ys = let z = had_v_to_int [ mod (x-y) nth_roots | (x,y) <- zip xs ys]
                   in orth_to_one_a ! z

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
is_valid :: HadNode -> Bool
-- is_valid (v:vs) = and [ are_orth v z | z <- vs]
-- is_valid (v:vs) = and [ are_orth_l v z | z <- vs]
is_valid (v:vs) = and [ are_orth_a v z | z <- vs]

make_valid_children :: HadNode -> [ HadNode ]
make_valid_children h = filter is_valid $ get_children h
-- make_valid_children h = filter is_valid $ get_children_n (next_v . next_v) h

make_valid_leafs :: HadNode -> [ HadNode ]
make_valid_leafs h = let ch = make_valid_children h
                     in if null ch
                        then if length h == dim
                             then [ h ]
                             else []
                        else concatMap make_valid_leafs ch
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
unit_ub :: [HadV]
unit_ub = filter (are_unbiased min_v)  all_vecs

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
all_mutually_unbiased_lines :: [[HadV]]
all_mutually_unbiased_lines = [ constrained_add (are_unbiased) [v] unit_ub  | v <- unit_ub ]

all_valid_hads = remove_equivs ( make_valid_leafs [ min_v ] )

-- A list of sets of mutually unbiased hads made from multiplying unbiased
-- lines with hadamard matrices
mubs_from_lines :: [[HadNode]]
mubs_from_lines = [ constrained_add (are_unbiased_had) [h]
                    [ d_mult diag h2 | diag <- diags, h2 <- all_valid_hads ]
                    | diags <- all_mutually_unbiased_lines,
                      h <- all_valid_hads ]
non_trivial_mubs = filter ((> 1) . length) mubs_from_lines

main = do
       -- print $ length $ nubBy pv_eq unit_ub
       -- print $ nubBy pv_eq unit_ub
       --Print all vectors which are unbiased to all ones and each other:
       -- print all_mutually_unbiased_lines
       -- print $ map length all_mutually_unbiased_lines
       --print non_trivial_mubs
       --print $ map length non_trivial_mubs
       encodeFile "all_valid_hads.bin" all_valid_hads
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