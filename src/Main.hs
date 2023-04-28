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
isValidGuess guess definition = map toLower guess == map toLower definition

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

    let correct = map toLower guess == map toLower definition
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

main :: IO ()
main = do 
    putStrLn "Enter filename: "
    filename <- getLine
    contents <- readFile filename

    let list = lines contents
        terms = getVocabTerms list

    guessAllUntilCorrect terms
