-- app/Main.hs (простая версия, без цикла)
module Main where

import Parser
import Glushkov
import Latex
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Regexp to DFA (Glushkov algorithm)║"
    putStrLn "Supported operations:"
    putStrLn "a, b, c, ...  - symbols"
    putStrLn "ab            - concatenation"
    putStrLn "a+b           - union"
    putStrLn "a*            - Kleene star"
    putStrLn "(...)         - grouping"
    putStrLn ""
    putStr "Enter regex: "
    input <- getLine
    
    if null input
        then putStrLn "No input provided."
        else processRegex input

processRegex :: String -> IO ()
processRegex input = do
    case parseRegex input of
        Left err -> do
            putStrLn "\n[ERROR] Failed to parse regex:"
            putStrLn (show err)
        Right re -> do
            putStrLn "\n[OK] Parsed successfully!"
            putStrLn ("  AST: " ++ show re)
            putStrLn ("  Pretty: " ++ regexToString re)
            
            let dfa = regexToDFA re
            
            putStrLn "\n[DFA] Built successfully!"
            putStrLn ("  Number of states: " ++ show (Set.size (states dfa)))
            putStrLn ("  Number of transitions: " ++ show (Map.size (transitions dfa)))
            
            let filename = "dfa_diagram.tex"
            saveLatex dfa filename
            putStrLn ("\n[LATEX] Diagram saved to: " ++ filename)