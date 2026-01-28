module Main ( main ) where

import Distribution.Simple
import Distribution.Simple.Program
import System.Process ( system )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { hookedPrograms = [checkBNFC]
    , preConf = \args configFlags -> do
        _ <- system "bnfc -p Syntax -o src -d src/Raw.bnf"
        -- remove the generated test file
        _ <- system "rm -f src/Syntax/Raw/Test.hs"
        _ <- system "rm -f src/Syntax/Raw/Doc.txt"
        _ <- system "rm -f src/Syntax/Raw/*.bak"
        preConf simpleUserHooks args configFlags
    }

checkBNFC :: Program
checkBNFC = (simpleProgram "bnfc") 
    { programFindVersion = findProgramVersion "--version" id }
