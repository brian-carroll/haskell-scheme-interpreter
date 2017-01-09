module Main where

-- Libraries
import System.Environment
import System.IO
import Control.Monad


-- Local modules
import Types
import Parser
import Eval


-- ----
-- MAIN
-- ----
main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ args !! 0
        otherwise -> putStrLn "Program takes only 0 or 1 argument"


-- -----
-- REPL
-- -----
flushStr :: String -> IO ()
flushStr str =
    putStr str >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt =
    flushStr prompt >> getLine


evalString :: String -> IO String
evalString expr =
    return $ extractValue $ trapError (liftM show (readExpr expr >>= eval))


evalAndPrint :: String -> IO ()
evalAndPrint expr =
    evalString expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action


runRepl :: IO ()
runRepl =
    until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint
