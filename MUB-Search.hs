

import System(getArgs)
import Data.Binary
import Data.Set

import Graph
import Magic

fixupBasis b = Data.Set.map (0 :) b

inverseVector v = Prelude.map (\x -> mod (12 - x) 12) v
productVector x y = zipWith (\x y -> mod (x + y) 12) x y

hasIdentity b = member (replicate 6 0) b
hasInverses b = all (\x -> member (inverseVector x) b) (elems b)
hasClosure  b = all (\x -> all (\y -> member (productVector x y) b) (elems b)) (elems b)


isSubGroup  b = (hasIdentity b) && (hasInverses b) && (hasClosure b)
isMonoid    b = (hasIdentity b) && (hasClosure b)
isSemigroup b = (hasClosure b)

identify    b = if   isSubGroup b
                then "subgroup"
                else if   isMonoid b
                     then "monoid"
                     else if   isSemigroup b
                          then "semigroup"
                          else "nothing"



type AdjFunc = Set Int
type Basis = Set Int


{-
  Add (mod 12) a vector to each vector in a basis.
-}
shiftBasis :: Basis -> Int -> Basis
shiftBasis b v = Data.Set.map (\z -> vec2magic (6, 12) $ addVecs vAsVec z) bAsVecs
                 where vAsVec = magic2vec (6, 12) v
                       bAsVecs = Data.Set.map (magic2vec (6, 12)) b
                       addVecs = Prelude.zipWith (\ x y -> mod (x + y) 12)


{-
  Given a basis b, construct the set of all coset bases to b.
-}
cosetBases :: AdjFunc -> Basis -> Set Basis
cosetBases adjBias b = Data.Set.map (shiftBasis b) adjBias


{-
  Determine if two bases are unbiased.
-}
areMUB :: Graph -> Basis -> Basis -> Bool
areMUB g b c = all areMUB' (elems b)
               where areMUB' v = all (\ u -> member u (g !! v)) $ elems c


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
  Given a basis b and set of bases r, construct all sets of n MUBs that include b.
-}
childMUBs :: Graph -> Basis -> Set Basis -> Int -> Set (Set Basis)
childMUBs g b r 0 = empty
childMUBs g b r n = iterate (biggerMUBsMany g r) (singleton (singleton b)) !! (n - 1)


main = do
--  arg_vlut : (arg_clut : arg_t) <- getArgs

  {-
    Read adjacency functions.
  -}
  adjOrth <- decodeFile "adjOrth12.bin" :: IO (Set Int)
  adjBias <- decodeFile "adjBias12.bin" :: IO (Set Int)


  {-
    Orthogonality graph and cliques.
  -}
  let gOrth = graph (6, 12) adjOrth
  let cOrth = rootcliques gOrth 6
  --print cOrth


  {-
    Unbiasedness graph and cliques.
  -}
  let gBias = graph (6, 12) adjBias
  let cBias = rootcliques gBias 6
  --print cBias

  let c = cOrth !! 0
  print $ childMUBs gBias c (cosetBases adjBias c) 2


  --dOrth <- decodeFile "cliques.bin" :: IO [Set Int]

  --let bOrth = Prelude.map (\b -> fixupBasis (Data.Set.map (magic2vec (6, 12)) b)) dOrth
  --let subgroups = (Prelude.filter (\x -> (identify x) == "subgroup") bOrth)
  --print $ take 3 subgroups
  --encodeFile "cliques.bin" $ s
  --print $ take 1 t

