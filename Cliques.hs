{-
  Find cliques on a special graph.

  Consider a graph in which vertices are tensors and the neighbors to a
  particular vertex can be computed by taking the pointwise division
  of that vertex (tensor) by all vectors adjacent to the 0-vector.

  If the elements in each tensor and vector are nth roots of unity then
  the pointwise division becomes pointwise subtraction of the root numbers.

  TODO

  2009 Nathan Blythe, Dr. Oscar Boykin (see LICENSE for details)
-}

import System(getArgs)
import Data.List

import Magic


type Eqp a = (a -> a -> Bool)
type Ngf a = (a -> [a])


{-
  The intersection of all lists in a list, performed pairwise repeatedly until
  finished, given an equivalence predicate.
-}
intersectionsBy :: Eqp a -> [[a]] -> [a]
intersectionsBy _ [] = []
intersectionsBy _ (h : []) = h
intersectionsBy p l = intersectionsBy p (f l)
                      where f [] = []
                            f (h : []) = [h]
                            f (h : g : t) = (intersectBy p h g) : f t


{-
  All size k super cliques of a clique q, given a list l of potential
  extending vertices.
-}
cliques' :: Eqp a -> Ngf a -> Integer -> ([a], [a]) -> [[a]]
cliques' _ _ 0 (q, _) = [q]
cliques' p n k (q, l) = concatMap (cliques' p n (k - 1)) s
                        where s = map (\ x -> (x : q, intersectBy p l (n x))) l


{-
  All size k cliques that include a clique from list qs.
-}
cliques :: Eqp a -> Ngf a -> Integer -> [[a]] -> [[a]]
cliques p n k qs = concatMap (\ q -> cliques' p n (k' q) (f q)) qs
                   where f q  = (q, intersectionsBy p (map n q))
                         k' q = k - (toInteger (length q))


{-
  Permutation-free list, given a function to compute a permutation-invariant of
  an object.
-}
permfree :: (Eq b) => (a -> b) -> [a] -> [a]
permfree f l = g [] l
               where g _ [] = []
                     g sl (h : t) = if   all (/= h') sl
                                    then h : g (h' : sl) t
                                    else g sl t
                                    where h' = f h


{-
  A map followed by a filter, performed simultaneously.
-}
milter :: (a -> b) -> (b -> Bool) -> [a] -> [b]
milter _ _ [] = []
milter f p (h : t) = if   p (f h)
                     then f h : milter f p t
                     else milter f p t


{-
  Given a function e to expand an object, a function x to combine two
  expanded objects, a function c to contract an object, and a list l of
  objects adjacent to the 0 object, compute the list of objects adjacent
  to an object v.
-}
neighbors :: Ord a => (a -> [a]) -> ([a] -> [a] -> [a]) -> ([a] -> a) -> [a] -> a -> [a]
neighbors e x c l v = milter f (> v) l
                      where f a = c (x (e v) (e a))


{-
  Compute the pointwise difference (mod n) between two lists.
-}
vecDiff :: (Integral a) => a -> [a] -> [a] -> [a]
vecDiff n = zipWith (\x y -> mod (x - y) n)


{-
  Determine how much work this particular process will do, and form the
  starts of the cliques that it will extend.
-}
specJobs :: Integer -> Integer -> [a] -> [a] -> [[a]]
specJobs s p z l | s <= 0               = [x : z | x <- l]
                 | (s < 0) || (s > nJ)  = error ("Job size out of range (" ++ (show nJ) ++ " jobs total)")
                 | (p < 0) || (p >= nP) = error ("Process index out of range (" ++ (show nP) ++ " processes total)")
                 | otherwise            = map (\x -> (genericIndex l x) : z) [p * s .. min ((p + 1) * s - 1) (nJ - 1)]
                   where nJ = toInteger (length l)
                         nP = (div nJ s) + (if mod nJ s == 0 then 0 else 1)


{-
  Cliques <d> <n> <fAdj> <r> <fTen> <k> <s> <p>

  Dimension d.
  nth roots of unity.
  Vectors adjacent to the 0-vector read from fAdj.
  Vertices are tensors of rank r.
  Generating set (under permutations) of vertex tensors read from fTen.
  Search for k-cliques.
  This process performs jobs p * s through (p + 1) * s - 1.
  If s == 0, the entire search is performed.
-}
main = do
  {-
    Command line arguments.
  -}
  d' : (n' : (fAdj : (r' : (fTen : (k' : (s' : (p' : argsT))))))) <- getArgs
  let d = read d' :: Integer
  let n = read n' :: Integer
  let r = read r' :: Integer
  let k = read k' :: Integer
  let s = read s' :: Integer
  let p = read p' :: Integer


  {-
    Common.
  -}
  ns' <- readFile fAdj
  vs' <- readFile fTen


  {-
    r = 1.
  -}
  let q1 = cliques (==)
                   (neighbors (magic2vec (d, n)) (vecDiff n) (vec2magic (d, n)) ns1)
                   k
                   (specJobs s p [0] vs1)
           where ns1 = map read (lines ns') :: [Integer]
                 vs1 = permfree (sort . magic2vec (d, n)) $ map read (lines vs') :: [Integer]


  {-
    r = 2.
  -}
  let q2 = cliques (\ x y -> (sort x) == (sort y))
                   (neighbors (magics2vecs (d, n)) (zipWith (vecDiff n)) (vecs2magics (d, n)) ns2)
                   k
                   (specJobs s p [] vs2)
           where ns2 = map ((genericTake d) . repeat . read) (lines ns') :: [[Integer]]
                 vs2 = permfree (transpose . sort . transpose . magics2vecs (d, n)) $ map read (lines vs') :: [[Integer]]


  {-
    Output.
  -}
  let s | k <= 2 = error ((show k) ++ "-cliques are boring")
        | r == 1 = map (putStrLn . show) q1
        | r == 2 = map (putStrLn . show) q2
  sequence_ $ s

