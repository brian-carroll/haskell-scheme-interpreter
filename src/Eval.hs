module Eval (eval, nullEnv) where

-- Libraries
import Control.Monad.Error

-- Local modules
import Types
import Eval.LispEnv
import Eval.WeakTyping


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Atom _) = return val
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env val@(DottedList head tail) = return val
eval env val@(List []) = return val

eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, conseq, alt]) = do 
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        otherwise -> eval env conseq
eval env (List (Atom "if" : badArgList)) = throwError $ NumArgs 3 badArgList

eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var

eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


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

    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)

    , ("eq?", eqv)
    , ("eqv?", eqv)
    , ("equal?", equal)
    ]





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
