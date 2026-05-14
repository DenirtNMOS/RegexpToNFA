-- Generate LaTeX code to visualize DFA
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Latex where

import Glushkov (DFA(..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List ( intercalate, foldl' )

-- Main function
generateLatex :: DFA -> String
generateLatex dfa = unlines
  [ "\\documentclass{standalone}"
  , "\\usepackage{tikz}"
  , "\\usetikzlibrary{automata,positioning,arrows}"
  , ""
  , "\\begin{document}"
  , "\\begin{tikzpicture}["
  , "  ->,"
  , "  >=stealth,"
  , "  node distance=2cm,"
  , "  auto,"
  , "  scale=0.8,"
  , "  every state/.style={thick, draw=black, fill=gray!5, minimum size=0.6cm, inner sep=0pt, font=\\small},"
  , "  initial text=$ $"
  , "]"
  , ""
  , generateStates dfa
  , ""
  , generateTransitions dfa
  , ""
  , "\\end{tikzpicture}"
  , "\\end{document}"
  ]

-- Generate states (Circle positioning)
generateStates :: DFA -> String
generateStates (DFA states start final _) =
  let
    stateList = Set.toList states
    n = length stateList
    stateToIdx = Map.fromList (zip stateList [0..])
    radius = 2.5

    positions :: [(Double, Double)]
    positions = if n == 1
        then [(0, 0)]
        else [ (radius * cos (2 * pi * fromIntegral i / fromIntegral n),
                radius * sin (2 * pi * fromIntegral i / fromIntegral n))
             | i <- [0..n-1] ]

    renderState :: (Set.Set Int, (Double, Double)) -> String
    renderState (s, (x, y)) =
      let idx = stateToIdx Map.! s
          isStart = (s == start)
          isFinal = Set.member s final
          stateName = "q" ++ show idx
          label = show idx
          posStr = "at (" ++ show x ++ "," ++ show y ++ ")"
      in case (isStart, isFinal) of
        (True, True)   -> "  \\node[state,initial,accepting] " ++ posStr ++ " (" ++ stateName ++ ") {" ++ label ++ "};"
        (True, False)  -> "  \\node[state,initial] " ++ posStr ++ " (" ++ stateName ++ ") {" ++ label ++ "};"
        (False, True)  -> "  \\node[state,accepting] " ++ posStr ++ " (" ++ stateName ++ ") {" ++ label ++ "};"
        (False, False) -> "  \\node[state] " ++ posStr ++ " (" ++ stateName ++ ") {" ++ label ++ "};"

  in
    unlines (zipWith (curry renderState) stateList positions)

-- Generate arrows
generateTransitions :: DFA -> String
generateTransitions dfa@(DFA states _ _ trans) =
  let
    stateList = Set.toList states
    stateToIdx = Map.fromList (zip stateList [0..])

    transitionsList :: [(Set.Set Int, Char, Set.Set Int)]
    transitionsList = [ (from, c, to) | ((from, c), to) <- Map.toList trans ]

    namedTransitions :: [(String, String, Char)]
    namedTransitions =
      [ ( "q" ++ show (stateToIdx Map.! from)
        , "q" ++ show (stateToIdx Map.! to)
        , c )
      | (from, c, to) <- transitionsList
      ]

    groupedTransitions :: [(String, String, String)]
    groupedTransitions =
      let grouped = foldl' addToGroup Map.empty namedTransitions
      in [ (from, to, intercalate "," syms) | ((from, to), syms) <- Map.toList grouped ]

    addToGroup :: Map.Map (String, String) [String] -> (String, String, Char) -> Map.Map (String, String) [String]
    addToGroup m (from, to, c) = Map.insertWith (++) (from, to) [[c]] m

    renderTransition :: (String, String, String) -> String
    renderTransition (from, to, syms)
      | from == to = "  \\path (" ++ from ++ ") edge [loop right, looseness=8] node {" ++ syms ++ "} (" ++ to ++ ");"
      | otherwise  = "  \\path (" ++ from ++ ") edge [bend left] node {" ++ syms ++ "} (" ++ to ++ ");"

  in
    if null groupedTransitions
    then "  % No transitions"
    else unlines (map renderTransition groupedTransitions)

-- Generate file
saveLatex :: DFA -> FilePath -> IO ()
saveLatex dfa path = do
  let content = generateLatex dfa
  writeFile path content
  putStrLn $ "LaTeX code saved to: " ++ path