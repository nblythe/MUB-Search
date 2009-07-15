

--import System.IO
import System(getArgs)
import Data.Binary
import Data.Set

import Graph
import Magic
import Perms


type Basis = Set Int



{-
  Inflate a basis from a set of vertices to a matrix of roots.
-}
inflateBasis :: Basis -> [[Int]]
inflateBasis b = Prelude.map (0 :) b''
                 where b'  = (insert 0 b)
                       b'' = elems $ magics2vecs (6, 12) b'


{-
  Determine if two bases are unbiased.
-}
areMUB :: Graph -> Basis -> Basis -> Bool
areMUB g b c = all areMUB' (elems b)
               where areMUB' v = all (\ u -> member (max u v) (g !! (min u v))) $ elems c


{-
  Compute the inverse of a vector.
-}
inverseVector v = Prelude.map (\x -> mod (12 - x) 12) v


{-
  Compute the product of two vectors.
-}
productVector x y = zipWith (\x y -> mod (x + y) 12) x y


{-
  Determine if a set s of vectors contains the identity element: the
  unity vector.
-}
hasIdentity s = member (replicate 6 0) s


{-
  Determine if a set s of vectors contains inverses of all vectors in s.
-}
hasInverses s = all (\x -> member (inverseVector x) s) (elems s)


{-
  Determine if a set s of vectors is closed under pointwise-multiplication.
-}
hasClosure  s = all (\x -> all (\y -> member (productVector x y) s) (elems s)) (elems s)


{-
  Determine if a basis b is a subgroup on the group of all vectors.
-}
isSubgroup b = (hasIdentity b') && (hasInverses b') && (hasClosure b')
               where b' = (fromList $ inflateBasis b)


{-
  Determine if a basis b is a monoid on the group of all vectors.
-}
isMonoid b = (hasIdentity b') && (hasClosure b')
             where b' = (fromList $ inflateBasis b)


{-
  Determine if a basis b is a semigroup on the group of all vectors.
-}
isSemigroup b = (hasClosure b')
                where b' = (fromList $ inflateBasis b)


{-
  Compute the intersection of all sets in a set.
-}
intersections s = if   1 == (Data.Set.size s)
                  then m
                  else intersection m (intersections t)
                  where (m, t) = deleteFindMin s


{-
  Given a set s of vertices, find the set of all vertices adjacent to all
  vertices in s on graph g.
-}

mutualNeighbors g s = intersections $ Data.Set.map (g !!) s





{-
  Given a basis b and a set s of vertices, is v*b mutually adjacent to b for
  all v in s?
-}
isCoAdjacent g b s = all (\ v -> areMUB g b $ shiftBasis b v) s










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
cosetBases :: Graph -> Basis -> Set Basis
cosetBases g b = Data.Set.map (shiftBasis b) (g !! 0)



main = do
  adjBias <- decodeFile "adjBias12.bin" :: IO (Set Int)
  let g = graphU (6, 12) adjBias


  bases <- decodeFile "newbases.bin" :: IO [Basis]
  let subgroups = Prelude.filter isSubgroup bases
  let neighbors = Prelude.map (\ h -> elems $ mutualNeighbors g h) subgroups
 
  --print $ Prelude.map (\ (x, s) -> isCoAdjacent g x s) $ zip subgroups neighbors
  

  let h = subgroups !! 0
  let v = neighbors !! 0 !! 0
  let h' = shiftBasis h v
  print h
  print $ Prelude.map (shiftBasis h) (neighbors !! 0)

  --print $ inflateBasis $ subgroups !! 0
--  print $ neighbors !! 6
--  print $ shiftBasis (subgroups !! 6) (neighbors !! 6 !! 0)
--  print $ shiftBasis (subgroups !! 6) (neighbors !! 6 !! 1)
--  print $ shiftBasis (subgroups !! 6) (neighbors !! 6 !! 2)
--  print $ shiftBasis (subgroups !! 6) (neighbors !! 6 !! 3)
--  print $ shiftBasis (subgroups !! 6) (neighbors !! 6 !! 4)
--  print $ shiftBasis (subgroups !! 6) (neighbors !! 6 !! 5)

  --print $ Prelude.map (\ (x, s) -> isCoAdjacent g x s) $ zip subgroups neighbors
  --let h = subgroups !! 0
  --let ub2h = mutualNeighbors g h
  --print $ shiftBasis h (elems ub2h !! 0)

  --print $ Prelude.map (\ h -> isCoAdjacent gBias h (elems $ mutualNeighbors gBias h)) subgroups
  --let ids = Prelude.map isSubgroup bases
  --print $ zip neighbors ids
  --print $ Prelude.map (\ h -> size $ mutualNeighbors gBias h) subgroups

  --let r = isCoAdjacent gBias h (elems ub2h)
--  let x = elems ub2h !! 1
--  let xh = shiftBasis h x
--  print $ areMUB gBias xh h
  --print r



  --let x = 7121 --findMin ub2h
  --let xh = shiftBasis h x
  --print $ member xh ub2h

  --print $ intersection adjBias (shiftBasis adjBias 942)

  --let c = bases !! 0
  --let r = cosetBases adjBias c
  --print $ size r
  --let r = fromList $ take 100 (elems $ cosetBases adjBias c)
  --print $ childMUBs gBias c r 2

  --let n = 2

  --putStr $ ("Searching for sets of " ++ (show n) ++ " MUBs...\n")
  --let mubs = findMUBs gBias bases n
  --putStr $ ("Found " ++ (show $ size mubs) ++ ".\n")




  --dOrth <- decodeFile "cliques.bin" :: IO [Set Int]

  --let bOrth = Prelude.map (\b -> fixupBasis (Data.Set.map (magic2vec (6, 12)) b)) dOrth
  --let subgroups = (Prelude.filter (\x -> (identify x) == "subgroup") bOrth)
  --print $ take 3 subgroups
  --encodeFile "cliques.bin" $ s
  --print $ take 1 t

