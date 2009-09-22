{-
  Find sets of MUBs amongst provided bases.

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}


import System(getArgs)
import Data.Binary
import Data.Set

import Graph
import Magic


{-
  Type definitions.
-}
type Basis = Set Int


{-
  Add (mod n) a vector to each vector in a basis.
-}
shiftBasis :: (Int, Int) -> Basis -> Int -> Basis
shiftBasis (d, n) b v = Data.Set.map (\z -> vec2magic (d, n) $ addVecs vAsVec z) $ Data.Set.map (magic2vec (d, n)) b
                        where vAsVec = magic2vec (d, n) v
                              addVecs = Prelude.zipWith (\ x y -> mod (x + y) n)


{-
  Given a basis b, construct the set of all coset bases to b.
-}
cosetBases :: (Int, Int) -> Graph -> Basis -> Set Basis
cosetBases (d, n) g b = Data.Set.map (shiftBasis (d, n) b) (g !! 0)


{-
  Determine if two bases are unbiased.
-}
areMUB :: Graph -> Basis -> Basis -> Bool
areMUB g b c = all areMUB' (elems b)
               where areMUB' v = all (\ u -> member (max u v) (g !! (min u v))) $ elems c


{-
  Determine if a basis is unbiased to all bases in a set.
-}
isMUBtoSet :: Graph -> Set Basis -> Basis -> Bool
isMUBtoSet g s b = all (areMUB g b) $ elems s


{-
  Extend a set s of mutually unbiased bases by a single basis from a set r to
  form new sets of bases.
-}
biggerMUBs :: Graph -> Set Basis -> Set Basis -> Set (Set Basis)
biggerMUBs g r s = Data.Set.map (\x -> insert x s) $ Data.Set.filter (isMUBtoSet g s) r


{-
  Extend a set x of sets of MUBs by a single basis from a set r.
-}
biggerMUBsMany :: Graph -> Set Basis -> Set (Set Basis) -> Set (Set Basis)
biggerMUBsMany g r x = unions $ Prelude.map (biggerMUBs g r) (elems x)


{-
  Given a basis b and set of bases r, construct all sets of n MUBs from r that include b.
-}
childMUBs :: Graph -> Basis -> Set Basis -> Int -> Set (Set Basis)
childMUBs g b r 0 = empty
childMUBs g b r n = iterate (biggerMUBsMany g r) (singleton (singleton b)) !! (n - 1)


{-
  Find all sets of k MUBs from bases in a list l.
-}
findMUBs :: (Int, Int) -> Graph -> [Basis] -> Int -> Set (Set Basis)
findMUBs (d, n) g l k = unions $ Prelude.map findMUBs' l
                        where findMUBs' c = childMUBs g c (allBases c) k
                              allBases = cosetBases (d, n) g
                              --allBases = unions $ Prelude.map (cosetBases (d, n) g) l


{-
  MUB-Search <d> <n> <fAdj> <fBases> <m> <fMUBs>
-}
main = do
  d : (n : (fAdj : (fBases : (m : (fMUBs : argsT))))) <- getArgs

  {-
    Read adjacency function and generate unbiased graph.
  -}
  putStr ("Reading unbiasedness relations from " ++ fAdj ++ ".\n")
  adjBias <- decodeFile fAdj :: IO (Set Int)
  let g = graph (read d, read n) adjBias


  {-
    Read bases.
  -}
  putStr ("Reading bases from " ++ fBases ++ ".\n")
  bases <- decodeFile fBases :: IO [Basis]


  {-
    Find MUBs and store to disk.
  -}
  putStr ("Writing sets of " ++ m ++ " MUBs to " ++ fMUBs ++ "...\n")
  let mubs = elems $ findMUBs (read d, read n) g bases (read m)
  encodeFile fMUBs mubs
  putStr ("Done; found " ++ (show $ length mubs) ++ " sets of " ++ m ++ " MUBs.\n")
  print $ Prelude.map (Data.Set.map (\ x -> magics2vecs (6, 12) x)) mubs

