module Main where

import Control.Cond
import Control.Monad
import Data.List
import Pipes
import Pipes.Prelude (toListM)
import Pipes.Tree
import Test.Hspec
import Test.Hspec.Expectations

main :: IO ()
main = hspec $ do
    describe "Sanity tests" $ do
        it "No tests yet" $ True `shouldBe` True
