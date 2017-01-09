module Main where

-- Libraries
import System.Environment
import System.IO
import Control.Monad


-- Local modules
import Types (Env, runIOThrows, liftThrows)
import Parser (readExpr)
import Eval (eval, nullEnv)


-- ----
-- MAIN
-- ----
main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ args !! 0
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


evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $   -- Convert IOThrowsError action into IO action
    liftM show $    -- Convert evaluated result to a string, within IOThrowsError
    (liftThrows $ readExpr expr) >>= eval env  -- Lift readExpr from ThrowsError into IOThrowsError, then evaluate


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =
    evalString env expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action


-- Create empty environment, then evaluate and print one expression
runOne :: String -> IO ()
runOne expr =
    nullEnv >>= flip evalAndPrint expr


-- Create empty environment, then apply (evalAndPrint env) to each line of input
runRepl :: IO ()
runRepl =
    nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
