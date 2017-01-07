module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric (readOct, readHex)
import qualified Data.Char


-- ----
-- MAIN
-- ----
main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show (readExpr (args !! 0) >>= eval)
     putStrLn $ extractValue $ trapError evaled


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


data LispError = NumArgs Integer [LispVal]
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


-- ----
-- EVAL
-- ----

eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval val@(DottedList head tail) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args)
        (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    -- , ("symbol?", typeCheckAtom)
    -- , ("string?", typeCheckString)
    -- , ("bool?", typeCheckBool)
    -- , ("number?", typeCheckNumber)
    -- , ("char?", typeCheckCharacter)
    -- , ("list?", typeCheckList)
    , ("symbol->string", symbolToString)
    , ("string->symbol", stringToSymbol)
    ]


symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom x] = return $ String x
symbolToString [val] = throwError $ TypeMismatch "Atom" val
symbolToString val = throwError $ NumArgs 1 val


stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol (String x : _) = return $ Atom x
stringToSymbol [val] = throwError $ TypeMismatch "String" val
stringToSymbol val = throwError $ NumArgs 1 val


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


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum


-- ------------------
-- HIGH LEVEL PARSING
-- ------------------
readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val


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
