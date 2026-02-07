module Main (main) where

import Distribution.Simple
import Text.Printf
import Distribution.Simple.Program
import System.Process (system)

main :: IO ()
main =
    defaultMainWithHooks
        simpleUserHooks
            { hookedPrograms = [checkBNFC]
            , preBuild = \args configFlags -> do
                _ <- system "make"
                preBuild simpleUserHooks args configFlags
            }

checkBNFC :: Program
checkBNFC =
    (simpleProgram "bnfc")
        { programFindVersion = findProgramVersion "--version" id
        }
