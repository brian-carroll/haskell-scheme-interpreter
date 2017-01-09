module Types
    ( LispVal (..)
    , LispError (..)
    , ThrowsError
    , trapError
    , extractValue
    , Env
    , IOThrowsError
    , liftThrows
    , runIOThrows
    )
    where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error, noMsg, strMsg, throwError, catchError, ErrorT, runErrorT)
import Data.IORef (IORef)


-- -----------
-- LISP VALUES
-- -----------

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Character Char
    | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "#\\" ++ [c]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList =
    unwords . map showVal



-- ------
-- ERRORS
-- ------

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values (" ++ unwordsList found ++ ")"
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


instance Show LispError where
    show = showError


instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default


type ThrowsError = Either LispError  -- partially applied


trapError action =
    catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) =
    val


-- --------------
-- LISP VARIABLES
-- --------------

-- Environment to store a map of Lisp variables
-- Lisp variables are mutable. Use IORef to update them inside IO monad
type Env = 
    IORef [(String, IORef LispVal)]


-- Type to allow us to throw LispErrors in the IO monad
type IOThrowsError =
    ErrorT LispError IO  -- partially applied, missing last type arg


-- Convert ThrowsError values into IOThrowsError values
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


-- Convert IOThrowsError actions into IO actions
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action =
    runErrorT (trapError action) >>= return . extractValue
