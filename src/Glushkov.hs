-- Glushkov's Algorithm
-- Builds DFA directly from regular expression
--
-- Steps:
-- 1. Add unique positions to each symbol
-- 2. Compute nullable, first, last
-- 3. Compute follow (position transitions)
-- 4. Build DFA states as sets of positions
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Glushkov where

import Parser (Regex(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Prelude hiding (map, last)

data Marked
  = MSym Int Char
  | MCat Marked Marked
  | MCup Marked Marked
  | MStar Marked
  deriving (Show, Eq)

type PosMap = Map Int Char

-- Add unique positions to each symbol
mark :: Regex -> (Marked, PosMap)
mark re = 
  let (m, map, _) = go re 1 Map.empty
  in (m, map)
  where
    go :: Regex -> Int -> PosMap -> (Marked, PosMap, Int)
    go (Sym c) i m = (MSym i c, Map.insert i c m, i + 1)
    go (Cat a b) i m =
      let (ma, m1, i1) = go a i m
          (mb, m2, i2) = go b i1 m1
      in (MCat ma mb, m2, i2)
    go (Cup a b) i m =
      let (ma, m1, i1) = go a i m
          (mb, m2, i2) = go b i1 m1
      in (MCup ma mb, m2, i2)
    go (Star a) i m =
      let (ma, m1, i1) = go a i m
      in (MStar ma, m1, i1)

-- nullable(A) = 1 if epsilon in A
nullable :: Marked -> Bool
nullable (MSym _ _) = False
nullable (MCat a b) = nullable a && nullable b
nullable (MCup a b) = nullable a || nullable b
nullable (MStar _)  = True

-- first(A) - first symbols of words in A
first :: Marked -> Set Int
first (MSym i _) = Set.singleton i
first (MCat a b) =
  if nullable a
    then Set.union (first a) (first b)
    else first a
first (MCup a b) = Set.union (first a) (first b)
first (MStar a) = first a

-- last(A) - last symbol of words in A
last :: Marked -> Set Int
last (MSym i _) = Set.singleton i
last (MCat a b) =
  if nullable b
    then Set.union (last a) (last b)
    else last b
last (MCup a b) = Set.union (last a) (last b)
last (MStar a) = last a

-- follow(a) - Set of positions that can follow 'a'
follow :: Marked -> Map Int (Set Int)
follow m = collect m Map.empty

-- build follow function
collect :: Marked -> Map Int (Set Int) -> Map Int (Set Int)
collect (MSym _ _) acc = acc
collect (MCat a b) acc =
  let
    acc1 = collect a acc
    acc2 = collect b acc1
    fromLast = last a
    toFirst = first b
    new = Set.foldl' (\map p -> Map.insertWith Set.union p toFirst map) acc2 fromLast
  in new
collect (MCup a b) acc =
  let
    acc1 = collect a acc
    acc2 = collect b acc1
  in acc2
collect (MStar a) acc =
  let
    acc1 = collect a acc
    fromLast = last a
    toFirst = first a
    new = Set.foldl' (\map p -> Map.insertWith Set.union p toFirst map) acc1 fromLast
  in new

data DFA = DFA
  { states      :: Set (Set Int)
  , startState  :: Set Int
  , finalStates :: Set (Set Int)
  , transitions :: Map (Set Int, Char) (Set Int)
  } deriving (Show, Eq)

buildDFA :: Marked -> PosMap -> DFA
buildDFA m pm =
  let
    start = first m
    finalPositions = last m
    followMap = follow m
    alphabet = Set.toList (Set.fromList (Map.elems pm))
    
    -- Transition function 
    delta :: Set Int -> Char -> Set Int
    delta state sym =
      Set.fromList
        [ p'
        | p <- Set.toList state
        , let next = Map.findWithDefault Set.empty p followMap
        , p' <- Set.toList next
        , Map.lookup p' pm == Just sym
        ]
    
    deltaWithStart :: Set Int -> Char -> Set Int
    deltaWithStart state sym
      | state == start =
          Set.filter (\p -> Map.lookup p pm == Just sym) state
      | otherwise =
          delta state sym
    
    reachable :: Set (Set Int) -> Map (Set Int, Char) (Set Int) -> (Set (Set Int), Map (Set Int, Char) (Set Int))
    reachable visited trans =
      let
        nextStates = Set.fromList
          [ deltaWithStart s sym
          | s <- Set.toList visited
          , sym <- alphabet
          , not (Set.null (deltaWithStart s sym))
          ]
        
        newStates = Set.difference nextStates visited
      in
        if Set.null newStates
        then (visited, trans)
        else
          let
            newTrans = Map.fromList
              [ ((s, sym), deltaWithStart s sym)
              | s <- Set.toList newStates
              , sym <- alphabet
              , not (Set.null (deltaWithStart s sym))
              ]
            allTrans = Map.union trans newTrans
            visited' = Set.union visited newStates
          in reachable visited' allTrans
    
    startTrans = Map.fromList
      [ ((start, sym), deltaWithStart start sym)
      | sym <- alphabet
      , not (Set.null (deltaWithStart start sym))
      ]
    
    (allStates, allTrans) = reachable (Set.singleton start) startTrans
    
    -- State is final if it have an element from last(regex)
    isFinal :: Set Int -> Bool
    isFinal s = not (Set.null (Set.intersection s finalPositions))
    
    final = Set.filter isFinal allStates
    
  in
    DFA allStates start final allTrans

-- Main function
regexToDFA :: Regex -> DFA
regexToDFA re =
  let (marked, pm) = mark re
  in buildDFA marked pm