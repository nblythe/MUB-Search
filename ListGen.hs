{-
  List generation based on selection from lists.
  2009 Nathan Blythe, Dr. Oscar Boykin
-}

module ListGen where

import List
import Ix

{-
  Add an element to a list of unique elements.  Equivalence
  between elements is determined by the predicate p.
-}
collectElement :: (a -> a -> Bool) -> [a] -> a -> [a]
collectElement p l v = if   (null (findIndices (p v) l))
                         then v : l
                         else l

{-
  Add elements from a list to a list of unique elements.
  Equivalence between elements is determined by the predicate p.
-}
collectElements p l [] = l
collectElements p l (v:vs) = collectElements p (collectElement p l v) vs

{-
  Consider a list of d elements, each taking on values from
  list l.  Construct the mth such list.
-}
constructIndices d n m = map (\x -> mod (div m (n^x)) n) (range (0, d - 1))
constructList d l m = map (\x -> (l !! (fromInteger x))) (constructIndices d (toInteger (length l)) m)

{-
  Consider a list of d elements, each taking on values from
  list l.  Construct all such lists.
-}
constructAllLists d l = map (constructList d l) (range(0, (toInteger (length l))^d - 1))

{-
  Consider a list of d elements, each taking on values from
  list l.  Construct all such unique lists.
-}
predSetEquiv x y = null (x \\ y)
constructAllUniqueLists d l = collectElements predSetEquiv [] (constructAllLists d l)

