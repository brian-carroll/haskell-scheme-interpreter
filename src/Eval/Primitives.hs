module Eval.Primitives (primitives) where

-- Library modules
import Control.Monad.Error (throwError)

-- Local modules
import LispTypes (LispVal (..), LispError (..), ThrowsError)
import Eval.WeakTyping (eqv, equal, unpackBool, unpackNum, unpackStr)


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
numericBinop _            []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_]  = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op
