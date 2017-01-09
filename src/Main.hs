{-# LANGUAGE ExistentialQuantification #-}

module Main where

-- Libraries
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Numeric (readOct, readHex)
import qualified Data.Char


-- Local modules
import Types


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
eval val@(List []) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function" func)
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

    , ("=", numBoolBinop (==))
    , ("<", numBoolBinop (<))
    , (">", numBoolBinop (>))
    , ("/=", numBoolBinop (/=))
    , (">=", numBoolBinop (>=))
    , ("<=", numBoolBinop (<=))
    , ("&&", boolBoolBinop (&&))
    , ("||", boolBoolBinop (||))
    , ("string=?", strBoolBinop (==))
    , ("string<?", strBoolBinop (<))
    , ("string>?", strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))

    , ("symbol?", typeMatch (Atom ""))
    , ("string?", typeMatch (String ""))
    , ("bool?", typeMatch (Bool True))
    , ("number?", typeMatch (Number 0))
    , ("char?", typeMatch (Character 'a'))
    , ("list?", typeMatch (List []))

    , ("symbol->string", symbolToString)
    , ("string->symbol", stringToSymbol)

    , ("if", conditional)
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)

    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = listEq eqv arg1 arg2
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


listEq :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> ThrowsError LispVal
listEq equalityFn arg1 arg2 =
    return $ Bool $
        (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case equalityFn [x1, x2] of
            Left err -> False
            Right (Bool val) -> val


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List arg1, List arg2] = listEq equal arg1 arg2
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $
                            mapM (unpackEquals arg1 arg2) 
                                [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ ((let (Bool x) = eqvEquals in x) || primitiveEquals)
equal badArgList = throwError $ NumArgs 2 badArgList



conditional :: [LispVal] -> ThrowsError LispVal
conditional [pred, conseq, alt] =
    do
        result <- eval pred
        case result of
            Bool False -> eval alt
            otherwise  -> eval conseq
conditional badArgList =
    throwError $ NumArgs 3 badArgList


car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


typeMatch :: LispVal -> [LispVal] -> ThrowsError LispVal
typeMatch (Atom _)         [Atom _]         = return $ Bool True
typeMatch (String _)       [String _]       = return $ Bool True
typeMatch (Bool _)         [Bool _]         = return $ Bool True
typeMatch (Number _)       [Number _]       = return $ Bool True
typeMatch (Character _)    [Character _]    = return $ Bool True
typeMatch (List _)         [List _]         = return $ Bool True
typeMatch (DottedList _ _) [DottedList _ _] = return $ Bool True
typeMatch _ [_] = return $ Bool False
typeMatch _ val = throwError $ NumArgs 1 val


symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom x] = return $ String x
symbolToString [val] = throwError $ TypeMismatch "Atom" val
symbolToString val = throwError $ NumArgs 1 val


stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol (String x : _) = return $ Atom x
stringToSymbol [val] = throwError $ TypeMismatch "String" val
stringToSymbol val = throwError $ NumArgs 1 val


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2 then
        throwError $ NumArgs 2 args
    else do
        left <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right


numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


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


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool


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
parseExpr =
    parseString
    <|> (try parseCharacter)
    <|> (try parseNumber)
    <|> parseAtom
    <|> parseQuoted
    <|> do
            char '('
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
symbol =
    oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces =
    skipMany1 space


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
