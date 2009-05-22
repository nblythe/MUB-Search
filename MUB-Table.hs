import Data.List
import Data.Complex
import Data.Array

{-
  Dimension in which we're working.

  Constant: integer
-}
dim :: Int
dim = 6


{-
  Roots of unity that will be checked.

  Constant: integer
-}
nth_roots :: Int
nth_roots = 12


{-
  Threshold for orthogonality and bias-ness.

  Constant: floating point number
-}
tiny :: Float
tiny = 0.0006181


{-
  Hadamard Vector

  Type definition: integer array
-}
type HadV = [Int] -- Hadamard Vector


{-
  Hadamard Node

  Type definition: Hadamard Vector array
-}
type HadNode = [HadV]


{- Converts k -> exp(2\pi i k/nth_roots) -}

{-
  Compute the kth nth_roots root of unity.
  (If nth_roots = 6 and k = 2, computes the second 6th root of unity)

  Function: integers -> complex floating point numbers
-}
int_to_comp :: Int -> Complex Float
int_to_comp k = cis $ 2.0 * pi * (fromIntegral k) / (fromIntegral nth_roots)


{-
  All nth_roots roots of unity.

  Constant: complex floating point array
-}
int_to_comp_a = listArray (-nth_roots, nth_roots) [ int_to_comp k | k <- range (-nth_roots, nth_roots)]


{-
  What is this?  Some sort of macro expansion?
-}
opmod :: Integral a => (a -> a -> a) -> a -> a -> a -> a
opmod f m x y = mod ( f x y ) m 
pmod = opmod (+)
mmod = opmod (-)


{-
  Compute the inner product of two Hadamard Vectors.

  Function: Hadamard Vectors x Hadamard Vectors -> complex floating point numbers
-}
ip :: HadV -> HadV -> Complex Float
ip xs ys = (sum $ map (int_to_comp_a !) [ x - y | (x,y) <- zip xs ys ]) + 1


{-
  Determine if two Hadamard Vectors are orthogonal.

  Function: Hadamard Vectors x Hadamard Vectors -> booleans

  Original comment: "Assuming that the HadV has i at position k
  meaning k is nth_roots of unity to the ith power"

  What does that mean?  This function just computes |<xs, ys>| and
  determines if it's smaller than the threshold.  I don't see any
  assumptions about elements of either vector.
-}
are_orth :: HadV -> HadV -> Bool
are_orth xs ys = (magnitude (ip xs ys)) < tiny


{-
  Determine if two Hadamard Vectors are unbiased.

  Function: Hadamard Vectors x Hadamard Vectors -> booleans

  Original comment: "We assume the first element is 1, the inner
  product should be \sqrt{d}"
-}
are_unbiased :: HadV -> HadV -> Bool
are_unbiased xs ys = abs( (magnitude (ip xs ys)) - sqrt (fromIntegral dim) ) < tiny


{-
  Determine if two Hadamard Nodes are unbiased (i.e. all possible
  combinations of vectors from each node are unbiased).

  Function: Hadamard Nodes x Hadamard Nodes -> booleans
-}
are_unbiased_had :: HadNode -> HadNode -> Bool
are_unbiased_had h1 h2 = and [ are_unbiased c1 c2 | c1 <- h1, c2 <- h2 ]


{-
  Compute the matrix product of a diagonal matrix (described by a Hadamard
  Vector) with a Hadamard Node.

  Function: Hadamard Vectors x Hadamard Nodes -> Hadamard Nodes
-}
d_mult :: HadV -> HadNode -> HadNode
d_mult v h = [ [mod (vi+wi) nth_roots | (vi, wi) <- zip v w] | w <- h]


{-
  The largest Hadamard Vector.

  Constant: Hadamard Vector
-}
max_v :: HadV
max_v = take (dim-1) (repeat (nth_roots - 1))


{-
  The smallest Hadamard Vector.

  Constant: Hadamard Vector
-}
min_v :: HadV
min_v = take (dim-1) (repeat 0)


{-
  Determine if a Hadamard Vector is the largest Hadamard Vector.

  Function: Hadamard Vectors -> booleans
-}
is_max :: HadV -> Bool
is_max = (== max_v)


{-
  Length of the largest Hadamard Vector.

  Constant: floating point number?
-}
l_max = length max_v


{-
  Determine if a Hadamard Vector is greater than or equal to
  the largest Hadamard Vector.

  Function: Hadamard Vectors -> booleans
-}
gt_e_max :: HadV -> Bool
gt_e_max v = (is_max v) || ((length v) > l_max)


{-
  Compute the next Hadamard Vector in some sequence.  What sequence?

  Function: Hadamard Vectors -> Hadamard Vectors

  If the vector is the null vector then the result is a vector with one element, 1.
  Otherwise:
    If the first element of the vector is less than nth_roots - 1 then the
    result is the vector with the first element incremented by 1.

    Otherwise the result is the vector with the first element replaced with
    0 and this function applied recursively to the tail of the vector.

  Original comment says this is "little-endian" addition, but what does this have
  to do with element ordering?  Is this just a discrete space-filling curve in the
  space of Hadamard Vectors with elements that are nth_roots of unity?
-}
next_v :: HadV -> HadV 
next_v [] = [1]
next_v (x:xs) = if x < nth_roots-1
                then (x+1):xs
                else 0 : next_v xs


{-
  Convert a Hadamard Vector to an integer.

  Function: Hadamard Vectors -> integers

  If the vector is the null vector then the result is 0.

  Otherwise the result is the first element plus nth_roots times
  the second element plus nth_roots^2 times the third element, and
  so on.
-}
had_v_to_int :: HadV -> Int
had_v_to_int [] = 0
had_v_to_int (x:xs) = x + nth_roots * had_v_to_int xs


{-
  All possible Hadamard Vectors.

  Constant: Hadamard Vector array

  Array is generated by starting with min_v, the smallest vector, and
  calling next_v repeatedly until no new vectors are greated less than
  or equal to max_v, the largest vector.
-}
all_vecs :: [HadV]
all_vecs = all_vecs' min_v
           where all_vecs' v | not $ is_max v = v : all_vecs' (next_v v)
                             | otherwise = v : []


{-
  All possible Hadamard Vectors orthogonal to [1, 1, 1 ... 1].

  Constant: Hadamard Vector array

  Original comment says that this array contains all vectors unbiased
  to "all 1s" but it looks like it is actually doing all vectors orthogonal
  to "all 1s", not unbiased.
-}
orth_to_one :: [HadV]
orth_to_one = map sort $ nubBy pv_eq (filter (are_orth min_v) all_vecs)


{-
  Determine whether two Hadamard Vectors are orthogonal.

  Function: Hadamard Vectors x Hadamard Vectors -> booleans

  Original comment suggests that this function, which is list-based,
  may be faster than are_orth.
-}
are_orth_l xs ys = let z = sort [ mod (x-y) nth_roots | (x,y) <- zip xs ys]
                   in elem z orth_to_one 


{-
  Integer numbering corresponding to all possible Hadamard Vectors
  orthogonal to [1, 1, 1 ... 1].

  Constant: integer array?

  While orth_to_one contains all vectors orthogonal to the "all 1s" vector,
  orth_to_one_a seems to contain the corresponding integer numbering.
-}
orth_to_one_a = listArray (had_v_to_int min_v, had_v_to_int max_v)
                          [ are_orth min_v x | x <- all_vecs ]


{-
  Determine whether two Hadamard Vectors are orthogonal

  Function: Hadamard Vectors x Hadamard Vectors -> booleans

  This seems to be another alternative to are_orth and are_orth_l.  This
  one uses the integer numbering of vectors.
-}
are_orth_a xs ys = let z = had_v_to_int [ mod (x-y) nth_roots | (x,y) <- zip xs ys]
                   in orth_to_one_a ! z


{-
  Generate all children of a given Hadamard Vector??

  Function: function (Hadamard Vectors -> Hadamard Vectors) x Hadamard Nodes
            -> Hadamard Vector arrays

  Takes a function and a Hadamard Node and produces an array of Hadamard Nodes.  The
  function is clearly expected to be some sort of "next" function and the Hadamard
  Node is an "initial" node I guess.

  If the length of the initial node is equal to dim then the result is the null vector.
  Otherwise the result is something to do with the "next" function applied to a vector
  constructed from the initial node.
-}
get_children_n :: (HadV -> HadV) -> HadNode -> [ HadNode ]
get_children_n next_f hn | length hn == dim = []
                | otherwise = g_c' hn [] $ head hn 
                where g_c' hn acc v | gt_e_max v = (v:hn):acc
                                    | otherwise = g_c' hn ((v:hn):acc) (next_f v)


{-
  Generate all children of... something?

  Function: ?? -> Hadamard Vector arrays

  Original comment is "get's all the children".  Children of what?  Seems to pass
  the function next_v to the function get_children_n... but get_children_n expects 
  a Hadamard Node as well; it looks like an argument is missing.  Maybe it assumes
  it to be null?
-}
get_children = get_children_n next_v


{-
  Determine if the last vector in a Hadamard Node is orthogonal to all other
  vectors in the node.

  Function: Hadamard Nodes -> booleans

  The original comment implies this is something we use as we add vectors to
  a node, so as to ensure that when we're done, all vectors in the node are
  mutually orthogonal.

  Note that there's a couple variants to select from, depending on which
  are_orth* is preferred.
-}
is_valid :: HadNode -> Bool
-- is_valid (v:vs) = and [ are_orth v z | z <- vs]
-- is_valid (v:vs) = and [ are_orth_l v z | z <- vs]
is_valid (v:vs) = and [ are_orth_a v z | z <- vs]


{-
  Generate all valid (consisting of mutually orthogonal vectors) children
  of a Hadamard Node.

  Function: Hadamard Nodes -> Hadamard Node arrays

  Note the commented out alternative that uses get_children_n directly.
-}
make_valid_children :: HadNode -> [ HadNode ]
make_valid_children h = filter is_valid $ get_children h
-- make_valid_children h = filter is_valid $ get_children_n (next_v . next_v) h


{-
  Generate all valid (consisting of mutually orthogonal vectors) leafs
  of a Hadamard Node.

  Function: Hadamard Nodes -> Hadamard Node arrays

  If the node has no valid children and the length of the node is equal
  to dim then the only valid leaf is the node itself.

  If the node has no valid children and the length of the node is not
  equal to dim then there are no valid leafs.

  If the node has valid children then the valid leafs are the results
  of make_valid_leafs applied recursively to these valid children.
-}
make_valid_leafs :: HadNode -> [ HadNode ]
make_valid_leafs h = let ch = make_valid_children h
                     in if null ch
                        then if length h == dim
                             then [ h ]
                             else []
                        else concatMap make_valid_leafs ch


{-
  Compute the factorial of a number.

  Function: integers -> integers
-}
fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n-1)


{-
  ??

  Function: integers x integers -> integers

  ??
-}
perm_list :: Int -> Int -> [Int]
perm_list 0 _ = []
perm_list elems n = let (d,m) = divMod n elems
                    in m:perm_list (elems - 1) d


{-
  ??

  Function: integer arrays x integers -> integers

  ??
-}
new_idx :: [Int] -> Int -> Int
new_idx [] idx = idx
new_idx (y:ys) idx = if idx >= y then new_idx ys (idx+1)
                     else new_idx ys idx


{-
  ??

  Function: integer arrays x integer arrays -> integer arrays

  ??
-}
pl_to_p' :: [Int] -> [Int] -> [Int]
pl_to_p' first [] = first
pl_to_p' first (x:xs) = let t_idx = new_idx (sort first) x
                            f' = first ++ [t_idx]
                        in pl_to_p' f' xs


{-
  pl_to_p' applied to null integer array.

  Constant: integer array
-}
pl_to_p = pl_to_p' []


{-
  ??

  Function: integers x integers -> integer arrays

  ??
-}
perm :: Int -> Int -> [Int]
perm n k = pl_to_p $ perm_list n k


{-
  ??

  Function: integers x integers x integer arrays -> integer arrays

  Original comment is just "Permute a vector".
-}
perm_v :: Int -> Int -> [Int] -> [Int]
perm_v n k v = [ v!!x | x <- perm n k]


{-
  Determine if two Hadamard Nodes are equivalent under left and right permutations.

  Function: integers x Hadamard Nodes x integers -> booleans?

  ??
-}
perm_h n had k = map (perm_v n k) had


{-
  Evaluate perm_h for all k between 0 and (n! - 1), inclusive.

  Function: integers x Hadamard Nodes -> boolean array?

  ??
-}
all_perm_had n had = map (perm_h n had) (range (0,(fac n)-1))


{-
  Generate all permutations of a given vector.

  Function: integers x Hadamard Vectors -> Hadamard Vector array
-}
all_perm_v :: Int -> HadV -> [HadV]
all_perm_v n v = map (flip (perm_v n) v) (range (0,(fac n)-1))


{-
  All Hadamard Vectors unbiased to the all unity vector.

  Constant: Hadamard Vector array
-}
unit_ub :: [HadV]
unit_ub = filter (are_unbiased min_v)  all_vecs


{-
  ??

  Function: ?? x ?? -> ??

  ??
-}
pv_eq x y = elem x (all_perm_v (dim-1) y)


{-
  Determine if two Hadamard Vectors (Nodes?) are identical.

  Function: Hadamard Vectors (Nodes?) x Hadamard Vectors (Nodes?) -> booleans
-}
same_h had1 had2 = null (had1 \\ had2)


{-
  Determine if two Hadamard Vectors (Nodes?) are equivalent, regardless
  of permutations.

  Function: integers x Hadamard Vectors (Nodes?) x Hadamard Vectors (Nodes?) -> booleans
-}
are_equiv n had1 had2 = let allh1 = all_perm_had n had1
                        in or $ map (same_h had2) allh1


{-
  Find the union (considering equivalence under permutations) of two
  Hadamard Node arrays.

  Function: Hadamard Node arrays x Hadamard Node arrays -> Hadamard Node arrays.
-}
remove_equivs' :: [HadNode] -> [HadNode] -> [HadNode]
remove_equivs' hs [] = hs
remove_equivs' [] (h:hs) = remove_equivs' [h] hs
remove_equivs' h0s (h:hs) = if or $ map (are_equiv (dim-1) h) h0s -- Already in the list
                            then remove_equivs' h0s hs
                            else remove_equivs' (h:h0s) hs -- Found a new one
remove_equivs hs = remove_equivs' [] hs
--remove_equivs = nubBy (are_equiv (dim-1))


{-
  Given a symmetric boolean function (f x y == f y x) and two arrays of
  possible inputs to each argument, generate an array of possible inputs such
  that the function is true for any pair selected from the array.

  Function: function (?? x ?? -> booleans) x ?? array x ?? array -> ?? array

  Does this work for any type "??"?
-}
constrained_add :: (a -> a -> Bool) -> [a] -> [a] -> [a]
constrained_add f x [] = x
constrained_add f [] (x:xs) = constrained_add f [x] xs
constrained_add f to (x:xs) = if and (map (f x) to) then constrained_add f (x:to) xs 
                              else constrained_add f to xs


{-
  All sets of vectors unbiased to the all unity vector.

  Constant: Hadamard Vector array array
-}
all_mutually_unbiased_lines :: [[HadV]]
all_mutually_unbiased_lines = [ constrained_add (are_unbiased) [v] unit_ub  | v <- unit_ub ]


{-
  All valid, unique (under permutations) Hadamard Vectors.

  Constant: Hadamard Vector array
-}
all_valid_hads = remove_equivs ( make_valid_leafs [ min_v ] )


{-
  All sets of mutually unbiased Hadamard Vectors, generated by multiplying
  all sets of vectors unbiased to the all unity vector against all valid, unique
  Hadamard Vectors.

  Constant: Hadamard Node array array
-}
mubs_from_lines :: [[HadNode]]
mubs_from_lines = [ constrained_add (are_unbiased_had) [h]
                    [ d_mult diag h2 | diag <- diags, h2 <- all_valid_hads ]
                    | diags <- all_mutually_unbiased_lines,
                      h <- all_valid_hads ]


{-
  All non-trivial (length > 1) Hadamard Nodes from those generated above.

  Constant: Hadamard Node array array
-}
non_trivial_mubs = filter ((> 1) . length) mubs_from_lines


{-
  Main routine: find and display all non-trivial MUBs as generated above.
-}
main = do
       -- print $ length $ nubBy pv_eq unit_ub
       -- print $ nubBy pv_eq unit_ub
       --Print all vectors which are unbiased to all ones and each other:
       -- print all_mutually_unbiased_lines
       -- print $ map length all_mutually_unbiased_lines
       print non_trivial_mubs
       print $ map length non_trivial_mubs
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

