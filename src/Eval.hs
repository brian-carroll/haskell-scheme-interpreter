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
-- import Debug.Trace (trace)

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
                argVals <- mapM (evalHelp False env) args
                if isTailContext
                    then
                        -- Tail call. Don't apply it just yet. Pop back up to outer `apply` first.
                        -- This prevents the recursion from getting too deep, which avoids stack overflow.
                        -- Too hard to refactor eval as tail recursive itself, so use trampoline style instead
                        return $ List (function : argVals)
                    else do
                        func <- eval env function
                        apply func argVals

        badForm ->
            throwError $ BadSpecialForm "Unrecognized special form" badForm




-- Apply function to args
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply lispfunc args =
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
                            zip params args)        -- 1. Match up param names with given input values
                            >>= bindVarArgs varargs -- 4. Combine varargs into the environment
                    mapM (eval env) (init body)     -- 5. Evaluate all forms except the tail expression (mutate env)
                    result <- evalTail env (last body)  -- 6. Evaluate tail expression
                    case result of
                        List (tailFuncName : tailCallArgs) ->
                            -- Scheme function ends with a tail call, which we want to optimise.
                            -- We have deliberately not evaluated the call yet, so as not to recurse too deep in `eval`.
                            -- Need Haskell code path to have obvious tail recursion, so GHC will optimise it.
                            -- (Pattern match could also match a list of values. Handle that later.)
                            do
                                tailCallFunc <- eval env tailFuncName
                                apply tailCallFunc tailCallArgs
                        _ ->
                            return result

        PrimitiveFunc func ->
            liftThrows $ func args

        IOFunc func ->
            func args

        _ ->
            -- If we end up here, it means we mistook a list for a tail call during the previous iteration.
            -- But that's OK, we've caught it now. Just reassemble the list and return it.
            return $ List (lispfunc : args)
