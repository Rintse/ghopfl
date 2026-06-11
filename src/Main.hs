module Main where

import Args
import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.Reader
import Preprocess.AnnotateVars
import Preprocess.Builtins
import Preprocess.Definitions
import Semantics.Evaluation
import Semantics.Typing
import Syntax.Parse
import Syntax.Types.Print as TypePrint
import System.Console.GetOpt
import System.Directory.Internal.Prelude (hPutStrLn)
import System.Environment (getArgs)
import System.Environment.Blank (getProgName)
import System.Exit
import System.IO (stderr)
import Tools.Treeify (showProg)

parseArgs :: IO (Options, String)
parseArgs = do
    args <- getArgs
    prog <- getProgName

    let header = "Usage: " ++ prog ++ " [OPTIONS] <script_file>"
    let helpMessage = usageInfo header Args.options

    case getOpt RequireOrder Args.options args of
        (optArgs, positionals, []) -> do
            opts <- foldl (>>=) (return defaultOpts) optArgs
            case positionals of
                (filename : _) -> return (opts, filename)
                _ -> do
                    let msg = "Missing required argument: `script_file`\n\n"
                    hPutStrLn stderr (msg ++ helpMessage)
                    exitFailure
        (_, _, errors) -> do
            hPutStrLn stderr (concat errors ++ helpMessage)
            exitFailure

main :: IO ()
main = do
    (opts, script_file) <- parseArgs
    let Options
            { optVerbose = verbosity
            , optEval = eval
            , optEnv = env
            , optDraws = draws
            , optDepth = depth
            } = opts

    raw <- Args.readFile script_file >>= parse verbosity
    let e2 = annotateVars raw
    -- e3 <- handleLetIns e2
    let e3 = e2
    let exp = e3

    -- case runExcept $ typeCheck exp of
    --     Left msg -> putStrLn $ "Program failed to type check: " ++ msg
    --     Right t -> putStrLn $ TypePrint.printTree t

    showProg verbosity exp
    when eval $
        evaluate verbosity exp depth draws env
