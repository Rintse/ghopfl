module Main where

import Args
import Control.Monad (unless, when)
import Control.Monad.Reader
import Preprocess.AnnotateVars
import Preprocess.Definitions
import Semantics.Evaluation
import Syntax.Parse
import Syntax.Types.Print as TypePrint
import Semantics.Typing
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import Tools.Treeify (showProg)
import Control.Monad.Except

parseArgs :: IO Options
parseArgs = do
    args <- getArgs
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    unless
        (null errs)
        ( do
            putStrLn "The were errors parsing the arguments:"
            mapM_ putStr errs
            exitFailure
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

    prog <- input >>= parse verb
    withDefinitions <- handleDefs prog
    let exp = annotateVars withDefinitions

    case runExcept $ typeCheck exp of
        Left msg -> putStrLn $ "Program failed to type check: " ++ msg
        Right t -> putStrLn $ TypePrint.printTree t

    showProg verb exp
    when eval $ 
        evaluate verb exp depth draws env
