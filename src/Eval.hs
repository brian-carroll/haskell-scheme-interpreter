module Eval
    ( eval
    , Env
    , primitiveBindings
    , liftThrows
    , runIOThrows
    )
    where

-- Libraries
import Control.Monad.Error (throwError, liftM)
import Control.Monad.Trans (liftIO)

-- Local modules
import LispTypes (LispVal (..), LispError (..))
import Eval.Env (getVar, setVar, defineVar, nullEnv, IOThrowsError, Env, liftThrows, runIOThrows, bindVars)
import Eval.Primitives (primitives)


-- Evaluate a Lisp expression
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _env val@(String _) = return val
eval _env val@(Number _) = return val
eval _env val@(Bool _) = return val
eval _env val@(Character _) = return val
eval _env val@(DottedList _ _) = return val
eval _env val@(List []) = return val

eval env (Atom id) = getVar env id

eval _env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, conseq, alt]) = do 
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _ -> eval env conseq
eval _env (List (Atom "if" : badArgList)) = throwError $ NumArgs 3 badArgList

eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body

eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals

eval _env badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm



-- Apply function to args
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args =
    liftThrows $ func args

apply (Func params varargs body closure) args =
        if wrongNumberOfArgs then
            throwError $ NumArgs (toInteger $ length params) args
        else
            (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        wrongNumberOfArgs =
            case varargs of
                Just _ ->
                    length args >= length params
                Nothing ->
                    length args /= length params

        remainingArgs =
            drop (length params) args
        bindVarArgs arg env =
            case arg of
                Just argName ->
                    liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing ->
                    return env

        evalBody env =               -- evaluate the function body
            liftM last $             -- last line of function gives its return value
                mapM (eval env) body -- evaluate each line of the function in the context of its env vars

apply _ _ = undefined


primitiveBindings :: IO Env
primitiveBindings =
        nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where
        makePrimitiveFunc (var, func) =
            (var, PrimitiveFunc func)


makeFunc varargs env params body =
    return $ Func (map show params) varargs body env

makeNormalFunc =
    makeFunc Nothing

makeVarArgs =
    makeFunc . Just . show
