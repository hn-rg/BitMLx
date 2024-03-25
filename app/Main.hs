module Main where

import ExampleRunner (runExample)
import qualified Examples.Escrow as Escrow
import qualified Examples.MutualTC as MutualTC
import qualified Examples.NaiveExchangeLottery as NaiveExchangeLottery
import qualified Examples.SimpleExchange as SimpleExchange
import qualified Examples.ReceiverChosenDenomination as ReceiverChosenDenomination

main :: IO ()
main = do
    runExample Escrow.example
    runExample MutualTC.example
    runExample NaiveExchangeLottery.example
    runExample SimpleExchange.example
    runExample ReceiverChosenDenomination.example
