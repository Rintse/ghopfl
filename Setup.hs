module Main (main) where

import Distribution.Simple
import Text.Printf
import Distribution.Simple.Program
import System.Process (system)

-- remove unneeded files
cleanBNFC :: String -> IO ()
cleanBNFC pfx = do
    putStrLn $ printf "Cleaning excess files in: %s" pfx
    system $ printf "rm -f %s/Test.hs" pfx
    system $ printf "rm -f %s/Doc.txt" pfx
    system $ printf "rm -f %s/*.bak" pfx
    return ()

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { hookedPrograms = [checkBNFC]
            , confHook = \args configFlags -> do
                putStrLn "Generating grammar for expressions"
                _ <- system "bnfc -p Syntax -o src -d src/Exp.bnf"

                putStrLn "Generating grammar for types"
                _ <- system "bnfc -p Syntax -o src -d src/Types.bnf"

                cleanBNFC "src/Syntax/Exp"
                cleanBNFC "src/Syntax/Types"
                confHook simpleUserHooks args configFlags
            }

checkBNFC :: Program
checkBNFC =
    (simpleProgram "bnfc")
        { programFindVersion = findProgramVersion "--version" id
        }
