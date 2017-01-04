module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn "I am a robot. What is your name?"
    userInput <- getLine
    putStrLn ("Hello, " ++ userInput)