module Eval
    ( eval
    , Env
    , primitiveBindings
    , liftThrows
    , runIOThrows
    , bindVars
    )
    where

-- Libraries
import Control.Monad.Error (throwError, liftM)
import Control.Monad.Trans (liftIO)
import Debug.Trace (trace)

-- Local modules
import LispTypes (LispVal (..), LispError (..))
import Eval.Env (getVar, setVar, defineVar, nullEnv, IOThrowsError, Env, liftThrows, runIOThrows, bindVars)
import Eval.Primitives (primitives, ioPrimitives, parseFile)


-- Action to be executed on startup
primitiveBindings :: IO Env
primitiveBindings =
        nullEnv >>= (flip bindVars (
                        map (makeFunc IOFunc) (ioPrimitives apply)
                        ++ map (makeFunc PrimitiveFunc) primitives
                    ))
    where
        makeFunc constructor (var, func) =
            (var, constructor func)


makeFunc varargs env params body =
    return $ Func (map show params) varargs body env

makeNormalFunc =
    makeFunc Nothing

makeVarArgs =
    makeFunc . Just . show



-- Evaluate a Lisp expression
eval :: Env -> LispVal -> IOThrowsError LispVal
eval = evalHelp False

evalTail :: Env -> LispVal -> IOThrowsError LispVal
evalTail = evalHelp True

evalHelp :: Bool -> Env -> LispVal -> IOThrowsError LispVal
evalHelp isTailContext env val =
    case val of
        String _ ->
            return val

        Number _ ->
            return val

        Bool _ ->
            return val

        Character _ ->
            return val

        DottedList _ _ ->
            return val

        List [] ->
            return val

        Atom varName ->
            getVar env varName

        List [Atom "quote", val] ->
            return val

        List [Atom "if", pred, conseq, alt] ->
            do
                result <- eval env pred
                case result of
                    Bool False ->
                        evalHelp isTailContext env alt
                    _ ->
                        evalHelp isTailContext env conseq

        List (Atom "if" : badArgList) ->
            throwError $ NumArgs 3 badArgList

        List [Atom "set!", Atom var, form] ->
            eval env form >>= setVar env var

        List [Atom "define", Atom var, form] ->
            eval env form >>= defineVar env var

        List (Atom "define" : List (Atom var : params) : body) ->
            makeNormalFunc env params body >>= defineVar env var

        List (Atom "define" : DottedList (Atom var : params) varargs : body) ->
            makeVarArgs varargs env params body >>= defineVar env var

        List (Atom "lambda" : List params : body) ->
            makeNormalFunc env params body

        List (Atom "lambda" : DottedList params varargs : body) ->
            makeVarArgs varargs env params body

        List (Atom "lambda" : varargs@(Atom _) : body) ->
            makeVarArgs varargs env [] body

        List [Atom "load", String filename] ->
            parseFile filename >>= liftM last . mapM (eval env)

        List (function : args) ->
            do
                func <- eval env function
                argVals <- mapM (evalHelp False env) args
                applyHelp isTailContext func argVals

        badForm ->
            throwError $ BadSpecialForm "Unrecognized special form" badForm




-- Apply function to args
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply = applyHelp False


-- Apply function to args
applyHelp :: Bool -> LispVal -> [LispVal] -> IOThrowsError LispVal
applyHelp isTailContext lispfunc args =
    case lispfunc of
        Func params varargs body closure ->
            let
                rightNumberOfArgs =
                    case varargs of
                        Just _ ->
                            length args >= length params
                        Nothing ->
                            length args == length params

                remainingArgs =
                    drop (length params) args

                bindVarArgs arg env =
                    case arg of
                        Just argName ->
                            liftIO $ bindVars env [(argName, List $ remainingArgs)]
                        Nothing ->
                            return env

            in
                if not rightNumberOfArgs then
                    throwError $ NumArgs (toInteger $ length params) args
                else do
                    env <- (liftIO $                -- 3. Lift from IO into IOThrowsError
                            bindVars closure $      -- 2. Combine the input parameters with the closure environment
                            -- trace ("tail: " ++ (show isTailContext) ++ "\t" ++ (show $ zip params args)) $ 
                            zip params args)        -- 1. Match up param names with given input values
                            >>= bindVarArgs varargs -- 4. Combine varargs into the environment
                    mapM (eval env) (init body)    -- 5. Evaluate all forms except the tail expression
                    evalTail
                        env --(trace (show env) env)
                        (last body)            -- 6. Evaluate tail expression

        PrimitiveFunc func ->
            liftThrows $ func args

        IOFunc func ->
            func args

        _ ->
            undefined
