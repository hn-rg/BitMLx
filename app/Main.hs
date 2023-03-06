module Main where

import Examples
import Runner (runCompiler)

participants = p4
preconditions = g4
contract = c4
outB = "examples/outB-put.rkt"
outD = "examples/outD-put.rkt"


main :: IO ()
main = runCompiler participants preconditions contract outB outD