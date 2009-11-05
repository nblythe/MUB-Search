--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
             FunctionalDependencies #-}

module Nub (StopList, nubWith, nub, nubOrd, nubInt)
where

import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

-- | In artificial intelligence, a 'StopList' is a
-- list of values that should be ignored in further
-- processing.  'nubWith' uses a stop list to filter
-- out duplicate elements.
class StopList e s | s -> e where
    emptyS :: s  -- ^ The empty stop list.
    memberS :: e -> s -> Bool  -- ^ Is e an element of s?
    insertS :: e -> s -> s  -- ^ Add e to s.  If e is already in s, the
                            -- result is undefined.

instance (Eq e) => StopList e [e] where
    emptyS = []
    memberS = elem
    insertS = (:)

instance (Ord e) => StopList e (Set.Set e) where
    emptyS = Set.empty
    memberS = Set.member
    insertS = Set.insert

instance StopList Int IntSet.IntSet where
    emptyS = IntSet.empty
    memberS = IntSet.member
    insertS = IntSet.insert

-- | The 'nubWith' function accepts as an argument
-- a "stop list", a set of list elements that it
-- uses as a filter to remove duplicate elements.
-- The supplied filter is normally initially empty.
nubWith :: (StopList e s) => s -> [e] -> [e]
nubWith _ [] = []
nubWith s (e : es)
    | memberS e s = nubWith s es
    | otherwise = e : nubWith (insertS e s) es

-- | The 'nub' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
-- (The name 'nub' means \`essence\'.)
-- It is a special case of 'nubBy', which allows the programmer to supply
-- their own equality test.  It is also a special case of 'nubWith', which
-- allows the programmer to specify their own "stop list".
nub :: (Eq e) => [e] -> [e]
nub = nubWith []

-- | The 'nubOrd' function is much more efficient on ordered
-- datatypes than 'nub'.  It is a special case of 'nubWith' with
-- a 'Set' stop list.
nubOrd :: (Ord e) => [e] -> [e]
nubOrd = nubWith Set.empty

-- | The 'nubInt' function is much more efficient on 
-- 'Int' lists than 'nub', and somewhat more efficient than
-- 'nubOrd'.  It is a special case of 'nubWith' with
-- an 'IntSet' stop list.
nubInt :: [Int] -> [Int]
nubInt = nubWith IntSet.empty
