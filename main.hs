module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    deriving (Show)

main :: IO ()
main =
    do
        (expr:_) <- getArgs
        putStrLn (readExpr expr)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right x -> "Found value" ++ show x


spaces :: Parser ()
spaces = skipMany1 space


escapedQuote :: Parser Char
escapedQuote =
    do
        char '\\'
        char '"'
        return '"'


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber


parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many (escapedQuote <|> (noneOf "\""))
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
