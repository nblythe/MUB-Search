

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


main = do
--  arg_vlut : (arg_clut : arg_t) <- getArgs

  {-
    Read vector tables and convert to sets for use as adjacency functions.
  -}
  vecsOrth <- decodeFile "orth12.bin" :: IO [Int]
  vecsBias <- decodeFile "unbias12.bin" :: IO [Int]
  let adjOrth = Data.Set.fromList vecsOrth
  let adjBias = Data.Set.fromList vecsBias


  {-
    Orthogonality graph and cliques.
  -}
  let gOrth = graph (6, 12) adjOrth
  let cOrth = rootcliques gOrth 6


  {-
    Unbiasedness graph and cliques.
  -}
  let gBias = graph (6, 12) adjBias
  let cBias = rootcliques gBias 7

  print $ take 5 (elems adjBias)

  dOrth <- decodeFile "cliques.bin" :: IO [Set Int]

  let bOrth = Prelude.map (\b -> fixupBasis (Data.Set.map (magic2vec (6, 12)) b)) dOrth
  let subgroups = (Prelude.filter (\x -> (identify x) == "subgroup") bOrth)
  print $ take 3 subgroups
  --encodeFile "cliques.bin" $ s
  --print $ take 1 t

