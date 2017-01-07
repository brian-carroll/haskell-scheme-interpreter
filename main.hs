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
unwordsList = unwords . map showVal


-- ----
-- MAIN
-- ----
main :: IO ()
main =
    getArgs >>= print . eval . readExpr . head


-- ----
-- EVAL
-- ----

eval :: LispVal -> LispVal
eval val@(Atom _) = val
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(DottedList head tail) = val
eval val@(List []) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply func args =
    maybe (Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("symbol?", typeCheckAtom)
    , ("string?", typeCheckString)
    , ("bool?", typeCheckBool)
    , ("number?", typeCheckNumber)
    , ("char?", typeCheckCharacter)
    , ("list?", typeCheckList)
    ]


typeCheckAtom :: [LispVal] -> LispVal
typeCheckAtom (Atom _ : _) = Bool True
typeCheckAtom _ = Bool False


typeCheckString :: [LispVal] -> LispVal
typeCheckString (String _ : _) = Bool True
typeCheckString _ = Bool False


typeCheckBool :: [LispVal] -> LispVal
typeCheckBool (Bool _ : _) = Bool True
typeCheckBool _ = Bool False


typeCheckNumber :: [LispVal] -> LispVal
typeCheckNumber (Number _ : _) = Bool True
typeCheckNumber _ = Bool False


typeCheckCharacter :: [LispVal] -> LispVal
typeCheckCharacter (Character _ : _) = Bool True
typeCheckCharacter _ = Bool False


typeCheckList :: [LispVal] -> LispVal
typeCheckList (List _ : _) = Bool True
typeCheckList _ = Bool False


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params =
    Number $ foldl1 op $ map unpackNum params


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0


-- ------------------
-- HIGH LEVEL PARSING
-- ------------------

readExpr :: String -> LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> String $ "No match: " ++ show err
        Right val -> val


parseList :: Parser LispVal
parseList =
    liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr = parseString
         <|> (try parseCharacter)
         <|> (try parseNumber)
         <|> parseAtom
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


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
