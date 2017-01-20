module Main where

-- Libraries
import System.Environment (getArgs)
import System.IO (hPutStrLn, stdout)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)

-- Local modules
import Parser (readMaybeExpr)
import Eval (eval, primitiveBindings, Env, runIOThrows, liftThrows, bindVars)
import LispTypes (LispVal (..))


-- ----
-- MAIN
-- ----
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            runRepl

        -- If first command line arg starts with '(' then assume it's a Lisp expression
        (('(' : _) : _) ->
            runOne $ args !! 0

        -- Otherwise assume it's a filename
        _ ->
            runFile args


-- Evaluate and print one expression
runOne :: String -> IO ()
runOne expr =
    primitiveBindings >>= flip evalAndPrint expr


-- Read a Scheme file and execute it
runFile :: [String] -> IO ()
runFile args =
    let
        firstArg =
            (args !! 0)
        otherArgsLispVal =
            List $ map String $ drop 1 args
    in do
        env <- primitiveBindings >>= flip bindVars [("args", otherArgsLispVal)] 
        loadLispFile env firstArg >>= hPutStrLn stdout


runRepl :: IO ()
runRepl =
    do
        putStrLn welcomeMessage
        env <- primitiveBindings
        loadLispFile env "stdlib.scm"
        runInputT defaultSettings (loop env)


loadLispFile :: Env -> String -> IO String
loadLispFile env filename =
    runIOThrows $ liftM show $ eval env $ List [Atom "load", String filename]


-- -----
-- REPL
-- -----

welcomeMessage :: String
welcomeMessage =
    "-------------------------------------------------\n" ++
    "|        Brian's Scheme Interpreter REPL        |\n" ++
    "-------------------------------------------------\n"


prompt :: String
prompt =
    "> "


loop :: Env -> InputT IO ()
loop env = do
    minput <- getInputLine prompt
    case minput of
        Nothing ->
            return ()

        Just "quit" ->
            return ()

        Just input ->
            do
                liftIO $ evalAndPrint env input
                loop env


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =
    evalString env expr >>= putStrLn


evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ do
        maybeLispVal <- liftThrows $ readMaybeExpr expr
        case maybeLispVal of
            Nothing ->
                return ""
            Just lispVal ->
                liftM show $ eval env lispVal
