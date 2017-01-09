module LispEnv where

-- Libraries
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.IORef

-- Local modules
import Types



nullEnv :: IO Env
nullEnv =
    newIORef []


isBound :: Env -> String -> IO Bool
isBound envRef var =
    readIORef envRef >>= return . maybe False (const True) . lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)  -- read value out of IO monad and lift it into whatever monad we're currently in
        (lookup var env)      -- look up the variable's value in the environment map


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value)) -- write value & lift action to current monad (IOThrowsError)
        (lookup var env)      -- look up the variable's value in the environment map
    return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value   -- create a new mutable *value* for the variable
            env <- readIORef envRef      -- dereference to get the latest environment state
            writeIORef envRef ((var, valueRef) : env)  -- mutate env, prepending new var name and reference to value
            return value   -- written value


-- Bind several vars at once (e.g. when a function is called)
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
        -- Get latest env value from its ref, prepend the new bindings, and create a new ref to new env value
        readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        -- Create list of new (name, ref) pairs and prepend to environment
        extendEnv bindings env =
            liftM (++ env) (mapM addBinding bindings)
        -- Create one new mutable variable ref and return (name, ref) pair
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)
