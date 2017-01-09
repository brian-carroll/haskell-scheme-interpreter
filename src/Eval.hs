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
import Eval.Env (setVar, defineVar, nullEnv, IOThrowsError, Env, liftThrows, runIOThrows)
import Eval.WeakTyping (eqv, equal, unpackBool, unpackNum, unpackStr)
import Eval.Primitives (primitives)


-- Evaluate a Lisp expression
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



-- Apply function to args
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function" func)
        ($ args)
        (lookup func primitives)


