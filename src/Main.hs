module Main (main) where

import Data.Char (toLower)
import System.Random (randomRIO)

import qualified Data.Vector as V

data VocabTerm = VocabTerm String String deriving Eq

instance Show VocabTerm where 
    show (VocabTerm t d) = t ++ ": " ++ d

reinforceAmount :: Int 
reinforceAmount = 3

greenColor :: String -> String 
greenColor str = "\x1b[32m" ++ str ++ "\x1b[0m"

yellowColor :: String -> String 
yellowColor str = "\x1b[33m" ++ str ++ "\x1b[0m"

redColor :: String -> String 
redColor str = "\x1b[31m" ++ str ++ "\x1b[0m"

getVocabTerms :: [String] -> V.Vector VocabTerm
getVocabTerms = V.fromList . map toVocabTerm
    where toVocabTerm str = VocabTerm first (tail second) 
            where (first, second) = break (== '\\') str

isValidGuess :: String -> String -> Bool 
isValidGuess guess definition = loweredGuess == loweredDefinition || editDistance loweredGuess loweredDefinition <= 2
    where loweredGuess = map toLower guess 
          loweredDefinition = map toLower definition

reinforceTerm :: Int -> VocabTerm -> IO ()
reinforceTerm 0 _ = return ()
reinforceTerm n vterm@(VocabTerm term definition) = do 

    putStrLn "-----------------------------------"
    putStrLn $ "[!] " ++ yellowColor term
    guess <- getLine

    if isValidGuess guess definition then
        putStrLn (greenColor "Correct") >>
        reinforceTerm (n - 1) vterm
    else
        putStrLn (redColor "Incorrect" ++ " - [" ++ definition ++ "]") >>
        reinforceTerm n vterm

guessAllUntilCorrect :: V.Vector VocabTerm -> IO ()
guessAllUntilCorrect terms = do
    termIndex <- randomRIO (0, length terms - 1)

    let vterm@(VocabTerm term definition) = terms V.! termIndex

    putStrLn "-----------------------------------"
    putStrLn $ "(?) " ++ term
    guess <- getLine

    let correct = isValidGuess guess definition
        nextTerms = V.filter (/= vterm) terms

    if correct then
        putStrLn (greenColor "Correct") >>
        if V.null nextTerms then
            putStrLn "You've guessed all the terms!"
        else
            guessAllUntilCorrect nextTerms
    else
        putStrLn (redColor "Incorrect" ++ " - [" ++ definition ++ "]") >>
        reinforceTerm reinforceAmount vterm >>
        guessAllUntilCorrect terms

-- Code from: https://stackoverflow.com/questions/5515025/edit-distance-algorithm-in-haskell-performance-tuning
-- standard levenshtein distance between two lists
editDistance :: Eq a => [a] -> [a] -> Int
editDistance s1 s2 = editDistance' 1 1 1 (V.fromList s1) (V.fromList s2)

-- weighted levenshtein distance
-- ins, sub and del are the costs for the various operations
editDistance' :: Eq a => Int -> Int -> Int -> V.Vector a -> V.Vector a -> Int
editDistance' del sub ins s1 s2
  | V.null s2 = ins * V.length s1
  | V.null s1 = ins * V.length s2
  | V.last s1 == V.last s2 = editDistance' del sub ins (V.init s1) (V.init s2)
  | otherwise            = minimum [ editDistance' del sub ins s1 (V.init s2)        + del -- deletion 
                                   , editDistance' del sub ins (V.init s1) (V.init s2) + sub -- substitution
                                   , editDistance' del sub ins (V.init s1) s2        + ins -- insertion
                                   ]

main :: IO ()
main = do 
    putStrLn "Enter filename: "
    filename <- getLine
    contents <- readFile filename

    let list = lines contents
        terms = getVocabTerms list

    guessAllUntilCorrect terms
