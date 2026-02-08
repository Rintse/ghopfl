module Syntax.Parse where

import Syntax.Exp.Abs
import Syntax.Exp.ErrM
import Syntax.Exp.Lex
import Syntax.Exp.Par
import Tools.VerbPrint

import Control.Exception
import Data.Typeable
import System.Exit

type ParseFun a = [Token] -> Err a
myLLexer = myLexer

type ParseMonad a = IO (Either SomeException a)

-- Custom parsing exception
data ParseException
    = DrawListException
    | EnvironmentException
    | DepthException
    deriving (Show, Typeable)
instance Exception ParseException

-- Parses contents of given input file
parse :: Int -> String -> IO Prg
parse v s = do
    putStrV v "Parsing program"
    let ts = myLLexer s
    case pPrg ts of
        Bad r -> do
            putStrLn $ "Parse failed: " ++ r
            putStrV v $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Ok r -> do
            putStrV v "Parse successful"
            return r

-- Used for builtins: we can just error out if this fails
parseExp :: String -> Exp
parseExp s = do
    let ts = myLLexer s
    case pExp ts of
        Bad r -> error $ 
            "Bulitin parse error: " ++ r 
            ++ "\nBuiltin:\n" ++ s
            ++ "\nTokens in stream:\n" ++ show ts
        Ok r -> r

-- Parses the environment if such an argument is given
parseEnv :: String -> IO Environment
parseEnv s = case pEnvironment (myLLexer s) of
    Bad s -> throw EnvironmentException
    Ok ev -> return ev
