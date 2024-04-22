{-
    To read the tree depth from BitMLx (.hs) contracts
-}
module Depth where
import Syntax.BitML
import Coins


depthC :: Coins c => Contract c -> Int
depthC c = maximum [depthD d | d <- c]

depthD :: Coins c => GuardedContract c -> Int
depthD d = case d of
    Put depositIds contract -> 1 + (depthC contract)
    Reveal secrets contract -> 1 + (depthC contract)
    RevealIf secrets pred contract -> 1 + (depthC contract)
    PutReveal depositIds secrets contract -> 1 + (depthC contract)
    PutRevealIf depositIds secrets pred contract -> 1 + (depthC contract)
    Withdraw p -> 1
    Auth ps guardedContract -> 1 + (depthD guardedContract)
    Split subcontracts -> 1 + sum [depthC contract | (_, contract) <- subcontracts]
    After time guardedContract -> depthD guardedContract