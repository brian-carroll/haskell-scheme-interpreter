module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


-- -----
-- TYPES
-- -----
data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)


-- ----
-- MAIN
-- ----
main :: IO ()
main =
    do
        (expr:_) <- getArgs
        putStrLn (readExpr expr)


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right (String x) -> "Found string value:\n" ++ x
    Right x -> "Found value" ++ show x


-- ------------------
-- HIGH LEVEL PARSING
-- ------------------

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber


parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many (escapedChar <|> (noneOf "\""))
        char '"'
        return $ String x


parseAtom :: Parser LispVal
parseAtom =
    do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $
            case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber =
    do
        numStr <- many1 digit
        let num = read numStr
        return $ Number num


-- ------------------
-- LOW-LEVEL PARSING
-- ------------------

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


escapedChar :: Parser Char
escapedChar =
    do
        char '\\'
        x <- oneOf "tnr\"\\"
        return $
            case x of
                '"' -> '"'
                '\\' -> '\\'
                't' -> '\t'
                'n' -> '\n'
                'r' -> '\r'
