module Main where

import Control.Cond
import Control.Monad.IO.Class
import Data.List
import Pipes
import Pipes.Tree

main :: IO ()
main = do
    let dir = "."
    let files = winnow (directoryFiles dir) $
            guard_ (".hs" `isInfixOf`)
    runEffect $ for (enumerate (walk files)) $ liftIO . print
