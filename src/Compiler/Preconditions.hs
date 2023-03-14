{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Compiler.Preconditions where

import Syntax.Common ( DCoins, BCoins, Deposit )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx

-- | Compiles a list of BitMLx contract preconditions into a list of
-- BitML preconditions for Bitcoin and one for Dogecoin.
compileG :: [BitMLx.G] -> ([BitML.G BCoins], [BitML.G DCoins])

compileG [] = ([], [])

-- | Deposits translate to deposits on the respective blockchains.
-- TODO: when we add the stipulation protocol, these should actually
-- translate to volatiles that will we put on the funding phase.
compileG (BitMLx.Deposit p (v1,v2) (x1,x2) : gs) = (gb, gd) where
    bDeposit = BitML.Deposit p v1 x1
    dDeposit = BitML.Deposit p v2 x2
    (gb', gd') = compileG gs
    gb = bDeposit : gb'
    gd = dDeposit : gd'

-- | Collaterals are implemented using regular deposits.
compileG (BitMLx.Collateral p (v1,v2) (x1,x2) : gs) = (gb, gd) where
    bDeposit = BitML.Deposit p v1 x1
    dDeposit = BitML.Deposit p v2 x2
    (gb', gd') = compileG gs
    gb = bDeposit : gb'
    gd = dDeposit : gd'
