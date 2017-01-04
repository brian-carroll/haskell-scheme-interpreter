module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show $ sum $ map read args
