-- Defines a simple alias for putStrLn which
-- additionally takes a verbosity parameter

module Tools.VerbPrint where

import Control.Monad (when)

putStrV :: Int -> String -> IO ()
putStrV v s = when ((> 0) v) $ putStrLn s
