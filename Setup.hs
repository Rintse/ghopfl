module Main (main) where

import Distribution.Simple
import Text.Printf
import Distribution.Simple.Program
import System.Process (system)

-- remove unneeded files
cleanBNFC :: String -> IO ()
cleanBNFC pfx = do
    putStrLn $ printf "Cleaning excess files in: %s" pfx
    system $ printf "rm -f %s/Raw/Test.hs" pfx
    system $ printf "rm -f %s/Raw/Doc.txt" pfx
    system $ printf "rm -f %s/Raw/*.bak" pfx
    return ()

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { hookedPrograms = [checkBNFC]
            , confHook = \args configFlags -> do
                putStrLn "Generating grammar for expressions"
                _ <- system "bnfc -p Syntax -o src -d src/Raw.bnf"

                putStrLn "Generating grammar for types"
                _ <- system "bnfc -p TypeSyntax -o src -d src/RawTypes.bnf"

                cleanBNFC "src/Syntax"
                cleanBNFC "src/TypeSyntax"
                confHook simpleUserHooks args configFlags
            }

checkBNFC :: Program
checkBNFC =
    (simpleProgram "bnfc")
        { programFindVersion = findProgramVersion "--version" id
        }
