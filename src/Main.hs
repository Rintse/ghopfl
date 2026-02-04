module Main where

import Args
import Control.Monad (unless, when)
import Control.Monad.Reader
import Preprocess.AnnotateVars
import Preprocess.Definitions
import Semantics.Evaluation
import Syntax.Parse
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Tools.Treeify

parseArgs :: IO Options
parseArgs = do
    args <- getArgs
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    unless
        (null errs)
        ( do
            putStrLn "The were errors parsing the arguments:"
            mapM_ putStr errs >> exitFailure
        )

    foldl (>>=) (return defaultOpts) optArgs

main :: IO ()
main = do
    opts <- parseArgs
    let Options
            { optVerbose = verb
            , optInput = input
            , optEval = eval
            , optEnv = env
            , optDraws = draws
            , optDepth = depth
            } = opts

    -- Parse input into a program AST
    prog <- input >>= parse verb
    -- Preprocess raw AST into one expression
    withDefinitions <- handleDefs prog
    let exp = annotateVars withDefinitions

    -- Show the result
    showProg verb exp
    -- Evaluate if requested
    when eval $ evaluate verb exp depth draws env
