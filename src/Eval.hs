module Eval
    ( eval
    , Env
    , nullEnv
    , liftThrows
    , runIOThrows
    )
    where

-- Libraries
import Control.Monad.Error (throwError)

-- Local modules
import LispTypes (LispVal (..), LispError (..), ThrowsError)
import Eval.Env (getVar, setVar, defineVar, nullEnv, IOThrowsError, Env, liftThrows, runIOThrows)
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

eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func

eval _env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



-- Apply function to args
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function" func)
        ($ args)
        (lookup func primitives)


