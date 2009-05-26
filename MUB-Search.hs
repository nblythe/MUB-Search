{-
  MUB-Search.hs

  Search a previously-constructed table for MUBs.
-}

import Data.List
import Data.Array
import System.IO
import System(getArgs)


{-
  Dimension we're working in.
-}
d :: Integer
d = 3
n :: Integer
n = 6


{-
  Type definition for vectors.
-}
type HadV = [Integer]


{-
  Type definition for matrices.
-}
type HadNode = [HadV]


{-
  Determine whether two vectors are orthogonal.
-}
are_orth :: HadV -> HadV -> Bool
are_orth xs ys = False


{-
  Determine if two vectors are unbiased.
-}
are_unbiased :: HadV -> HadV -> Bool
are_unbiased xs ys = False


{-
  Determine if two matrices are vector-wise unbiased.
-}
are_unbiased_had :: HadNode -> HadNode -> Bool
are_unbiased_had h1 h2 = False


{-
  Treat a vector like a diagonal matrix and multiply by another matrix.
-}
d_mult :: HadV -> HadNode -> HadNode
d_mult v h = [ [mod (vi+wi) n | (vi, wi) <- zip v w] | w <- h]


{-
  The largest vector.
-}
max_v :: HadV
max_v = take (fromInteger (d - 1)) (repeat (n - 1))


{-
  The smallest vector, the unity vector.
-}
unity_v :: HadV
unity_v = take (fromInteger (d - 1)) (repeat 0)


{-
  Determine if a vector is the largest vector.
-}
is_max :: HadV -> Bool
is_max = (== max_v)


{-
  Determine if a vector is larger or equal to the largest
  vector.
-}
gt_e_max :: HadV -> Bool
gt_e_max v = (is_max v) || ((length v) > (length max_v))


{-
  Compute the next vector in the sequence.
-}
next_v :: HadV -> HadV 
next_v [] = [1]
next_v (x : xs) = if x < n - 1
                then (x + 1):xs
                else 0 : next_v xs



{- Convert a HadV to a integer
 -}
had_v_to_int :: HadV -> Integer
had_v_to_int [] = 0
had_v_to_int (x:xs) = x + n * had_v_to_int xs


{-
  All possible vectors.
-}
all_vecs :: [HadV]
all_vecs = all_vecs' unity_v
           where all_vecs' v | not $ is_max v = v : all_vecs' (next_v v)
                             | otherwise = v : []



{-
  Factorial
-}
fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)

{-
  A list of permutations?
-}
perm_list :: Integer -> Integer -> [Integer]
perm_list 0 _ = []
perm_list elems n = let (d, m) = divMod n elems
                    in m : perm_list (elems - 1) d

{-
  ??
-}
new_idx :: [Integer] -> Integer -> Integer
new_idx [] idx = idx
new_idx (y:ys) idx = if idx >= y then new_idx ys (idx+1)
                     else new_idx ys idx


{-
  ??
-}
pl_to_p' :: [Integer] -> [Integer] -> [Integer]
pl_to_p' first [] = first
pl_to_p' first (x:xs) = let t_idx = new_idx (sort first) x
                            f' = first ++ [t_idx]
                        in pl_to_p' f' xs
pl_to_p = pl_to_p' []


{-
  ??
-}
perm :: Integer -> Integer -> [Integer]
perm n k = pl_to_p $ perm_list n k


{-
  Permute a vector.
-}
perm_v :: Integer -> Integer -> [Integer] -> [Integer]
perm_v n k v = [ v !! (fromInteger x) | x <- perm n k]


{-
  Determine if two matrices are equivalent under left or right
  permutations.
-}
perm_h n had k = map (perm_v n k) had
all_perm_had n had = map (perm_h n had) (range (0, (fac n) - 1))


{-
  All possible permutations of a given vector.
-}
all_perm_v :: Integer -> HadV -> [HadV]
all_perm_v n v = map (flip (perm_v n) v) (range (0,(fac n)-1))


{-
  Determine if two vectors are equivalent under some permutation.
-}
pv_eq x y = elem x (all_perm_v (d-1) y)


{-
  Determine if two vectors are orthogonal.
-}
are_orth_a xs ys = (vec_table !! (fromInteger (had_v_to_int [ mod (x-y) n | (x,y) <- zip xs ys]))) !! 0


{-
  Determine if two vectors are unbiased.
-}
are_unbiased_a xs ys = (vec_table !! (fromInteger (had_v_to_int [ mod (x-y) n | (x,y) <- zip xs ys]))) !! 1


{-
  Construct all of the children of a given matrix.
-}
get_children_n :: (HadV -> HadV) -> HadNode -> [ HadNode ]
get_children_n next_f hn | length hn == (fromInteger d) = []
                | otherwise = g_c' hn [] $ head hn 
                where g_c' hn acc v | gt_e_max v = (v:hn):acc
                                    | otherwise = g_c' hn ((v:hn):acc) (next_f v)


{-
  ??
-}
get_children = get_children_n next_v


{-
  Determine if the last vector in a matrix is orthogonal to all of
  the others.
-}
is_valid :: HadNode -> Bool
is_valid (v:vs) = and [ are_orth_a v z | z <- vs]


{-
  Construct all valid children of a matrix.
-}
make_valid_children :: HadNode -> [ HadNode ]
make_valid_children h = filter is_valid $ get_children h


{-
  Make all valid leafs of a matrix.
-}
make_valid_leafs :: HadNode -> [ HadNode ]
make_valid_leafs h = let ch = make_valid_children h
                     in if null ch
                        then if length h == (fromInteger d)
                             then [ h ]
                             else []
                        else concatMap make_valid_leafs ch


{-
  All vectors that are unbiased to the unity vector.
-}
unit_ub :: [HadV]
unit_ub = filter (are_unbiased_a unity_v)  all_vecs


{-
  Determine if two matrices are identical.
-}
same_h had1 had2 = null (had1 \\ had2)


{-
  Determine if two matrices are equivalent under permutations.
-}
are_equiv n had1 had2 = let allh1 = all_perm_had n had1
                        in or $ map (same_h had2) allh1


{-
  Construct the union (under equivalence) of two lists of
  matrices.
-}
remove_equivs' :: [HadNode] -> [HadNode] -> [HadNode]
remove_equivs' hs [] = hs
remove_equivs' [] (h:hs) = remove_equivs' [h] hs
remove_equivs' h0s (h:hs) = if or $ map (are_equiv (d-1) h) h0s -- Already in the list
                            then remove_equivs' h0s hs
                            else remove_equivs' (h:h0s) hs -- Found a new one
remove_equivs hs = remove_equivs' [] hs


{-
  For some symmetric boolean function (f x y == f y x), add
  each item so that the function is true between all pairs:
-}
constrained_add :: (a -> a -> Bool) -> [a] -> [a] -> [a]
constrained_add f x [] = x
constrained_add f [] (x:xs) = constrained_add f [x] xs
constrained_add f to (x:xs) = if and (map (f x) to) then constrained_add f (x:to) xs 
                              else constrained_add f to xs


{-
  All sets of vectors unbiased to the unity vector.
-}
all_mutually_unbiased_lines :: [[HadV]]
all_mutually_unbiased_lines = [ constrained_add (are_unbiased) [v] unit_ub  | v <- unit_ub ]


{-
  All valid matrices: valid leafs with equivalent matrices removed.
-}
all_valid_hads = remove_equivs ( make_valid_leafs [ unity_v ] )


{-
  Mutually unbiased bases formed by multiplying unbiased valid matrices
  (from above) by all valid matrices.

  TODO: what?
-}
mubs_from_lines :: [[HadNode]]
mubs_from_lines = [ constrained_add (are_unbiased_had) [h]
                    [ d_mult diag h2 | diag <- diags, h2 <- all_valid_hads ]
                    | diags <- all_mutually_unbiased_lines,
                      h <- all_valid_hads ]


{-
  Mutually unbiased bases with trivial bases removed.
-}
non_trivial_mubs vt = filter ((> 1) . length) mubs_from_lines


{-
  Convert a 0 or 1 to a boolean.
-}
bit2bool :: String -> Bool
bit2bool x = case x of
                  "1" -> True
                  "0" -> False

{-
  Convert a list of 0s or 1s to a list of booleans.
-}
bits2bools :: [String] -> [Bool]
bits2bools x = map bit2bool x



{-
  Main routine: find and display all non-trivial MUBs as generated above.
-}
main = do
  x <- readFile "exact.txt"
  let vec_table = map bits2bools (map words (lines x))
  

--vec_table :: [[Bool]]
--vec_table = 

  --argH : argT <- getArgs
  --readFile argH (vec_table)

  --print "All non-trivial MUBs:"
  --print non_trivial_mubs
  --     print $ map length non_trivial_mubs
  putStr (show $ length y)
  --putStr ("Found " ++ (show $ length non_trivial_mubs))
  --putStr (" sets, the largest having " ++ (show $ maximum $ map length non_trivial_mubs))
  --putStr " mutually unbiased bases.\n\n"

  --putStr "All non-trivial MUBs:\n"
  --putStr ((show non_trivial_mubs) ++ "\n\n")

  --putStr ("Number of bases in each: " ++ (show $ map length non_trivial_mubs) ++ "\n")

