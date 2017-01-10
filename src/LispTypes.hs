module LispTypes
    ( LispVal (..)
    , LispError (..)
    , ThrowsError
    , Env
    )
    where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error, noMsg, strMsg)
import Data.IORef (IORef)


-- Environment to store a map of Lisp variables
-- Lisp variables are mutable. Use IORef to update them inside IO monad
type Env =
    IORef [(String, IORef LispVal)]


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
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func  { params :: [String]        -- parameter names
            , vararg :: (Maybe String)  -- variable-length arg list
            , body :: [LispVal]         -- function body, as a list of Lisp forms
            , closure :: Env            -- environment the function was created in
            }

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "#\\" ++ [c]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _body, closure = _env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg
        ) ++ ") ...)"


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
showError _                             = undefined


instance Show LispError where
    show = showError


instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default


type ThrowsError = Either LispError  -- partially applied
