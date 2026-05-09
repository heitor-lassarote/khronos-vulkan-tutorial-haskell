module Main (main) where

import Distribution.Simple
import System.Process (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> do
    callProcess "bash" ["shaders/compile.sh"]
    preBuild simpleUserHooks args flags
  }
