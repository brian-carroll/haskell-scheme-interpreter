module Eval.Primitives
        ( primitives
        , ioPrimitives
        , parseFile
        )
    where

-- Library modules
import Control.Monad.Error (throwError)
import System.IO (IOMode (ReadMode, WriteMode), hGetLine, hPrint, hClose, stdin, stdout, openFile)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)

-- Local modules
import LispTypes (LispVal (..), LispError (..), ThrowsError, IOThrowsError)
import Eval.WeakTyping (eqv, equal, unpackBool, unpackNum, unpackStr)
import Eval.Env (liftThrows)
import Parser (readExpr, readExprList)


-- IO primitives
ioPrimitives :: (LispVal -> [LispVal] -> IOThrowsError LispVal) -> [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives apply =
    [ ("apply", applyProc apply)
    , ("open-input-file", makePort ReadMode)
    , ("open-output-file", makePort WriteMode)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("read", readProc)
    , ("write", writeProc)
    , ("read-contents", readContents)
    , ("read-all", readAll)
    ]


applyProc :: (LispVal -> [LispVal] -> IOThrowsError LispVal) -> [LispVal] -> IOThrowsError LispVal
applyProc apply [func, List args] = apply func args
applyProc apply (func : args)     = apply func args
applyProc _ _ = undefined


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = undefined


closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False


readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc _ = undefined


writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _ = undefined


readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents _ = undefined


parseFile :: String -> IOThrowsError [LispVal]
parseFile filename = (liftIO $ readFile filename) >>= liftThrows . readExprList


readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ parseFile filename
readAll _ = undefined


-- Purely functional primitives

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

    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)

    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]


car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
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
stringToSymbol [String x] = return $ Atom x
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
numericBinop op params        =
    if length params /= 2 then
        throwError $ NumArgs 2 params
    else do
        left <- unpackNum $ params !! 0
        right <- unpackNum $ params !! 1
        return $ Number $ left `op` right
