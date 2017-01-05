module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readOct, readHex)
import qualified Data.Char


-- -----
-- TYPES
-- -----
data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Character Char
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
    Right x -> "Found value:\n" ++ show x


-- ------------------
-- HIGH LEVEL PARSING
-- ------------------

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> (try parseCharacter)
         <|> (try parseNumber)
         <|> parseAtom


parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many (escapedChar <|> (noneOf "\""))
        char '"'
        return $ String x


parseCharacter :: Parser LispVal
parseCharacter =
    do
        char '#'
        char '\\'
        c <- (try characterName <|> anyChar)
        return $ Character c


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
        value <- (radixNumber <|> plainNumber)
        return $ Number value


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


plainNumber :: Parser Integer
plainNumber =
    do
        numStr <- many1 digit
        let value = read numStr
        return value


radixNumber :: Parser Integer
radixNumber =
    do
        char '#'
        value <- (hexNumber <|> octalNumber <|> decimalNumber <|> binaryNumber)
        return value


hexNumber :: Parser Integer
hexNumber =
    do
        oneOf "Xx"
        numStr <- many1 $ oneOf "0123456789abcdefABCDEF"
        let [(value, _)] = readHex numStr
        return value


octalNumber :: Parser Integer
octalNumber =
    do
        oneOf "Oo"
        numStr <- many1 $ oneOf "01234567"
        let [(value, _)] = readOct numStr
        return value


decimalNumber :: Parser Integer
decimalNumber =
    do
        oneOf "Dd"
        numStr <- many1 $ digit
        let value = read numStr
        return value


binaryNumber :: Parser Integer
binaryNumber =
    do
        oneOf "Bb"
        numStr <- many1 $ oneOf "01"
        let value = readBin numStr
        return value


readBin :: String -> Integer
readBin str =
    let
        (value, _) = foldr readBinHelp (0,1) str
    in
        value


readBinHelp :: Char -> (Integer, Integer) -> (Integer, Integer)
readBinHelp char (acc, bitWeight) =
    let
        bitValue =
            if char == '1' then
                bitWeight
            else
                0
    in
        ( acc+bitValue, 2*bitWeight )


characterName :: Parser Char
characterName =
    do
        first <- letter
        rest <- many1 $ letter
        let name = map Data.Char.toLower (first:rest)

        -- http://sicp.ai.mit.edu/Fall-2003/manuals/scheme-7.5.5/doc/scheme_6.html
        let c = case name of
                    "altmode" -> '\ESC'
                    "backnext" -> '\US'
                    "backspace" -> '\BS'
                    "call" -> '\SUB'
                    "linefeed" -> '\LF'
                    "newline" -> '\LF'
                    "page" -> '\FF'
                    "return" -> '\CR'
                    "rubout" -> '\DEL'
                    "tab" -> '\HT'
                    "space" -> ' '
                    _ -> '_'

        when (c=='_') (unexpected ("character name '" ++ name ++ "'"))
        return c
