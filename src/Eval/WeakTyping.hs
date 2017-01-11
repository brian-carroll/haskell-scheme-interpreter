{-# LANGUAGE ExistentialQuantification #-}

module Eval.WeakTyping
    ( eqv
    , equal
    , unpackBool
    , unpackNum
    , unpackStr
    )
    where

import LispTypes (LispVal (..), LispError (..), ThrowsError)
import Control.Monad.Error (throwError, catchError, liftM)


-- Typeclass to help with weak typing
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)


-- Strictly typed equality
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = listEq eqv arg1 arg2
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


-- Weakly typed equality
equal :: [LispVal] -> ThrowsError LispVal
equal [List arg1, List arg2] = listEq equal arg1 arg2
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $
                            mapM (unpackEquals arg1 arg2) 
                                [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ ((let (Bool x) = eqvEquals in x) || primitiveEquals)
equal badArgList = throwError $ NumArgs 2 badArgList



-- Helper function to recursively test equality of 2 lists
listEq :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> ThrowsError LispVal
listEq equalityFn arg1 arg2 =
    return $ Bool $
        (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where
        eqvPair (x1, x2) =
            case equalityFn [x1, x2] of
                Left _ -> False
                Right (Bool val) -> val
                Right _ -> undefined


-- Helper function to test equality under a particular unpacking function
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)



-- Unpacking functions
--  Type conversions to implement weak typing

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

