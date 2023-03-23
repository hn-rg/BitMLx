{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Compiler.Preconditions where

import Coins ( DCoins, BCoins)
import Syntax.Common ( Deposit )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Syntax.BitMLx (G(StepSecret))

-- | Compiles a BitMLx contract precondition into a
-- BitML preconditions for Bitcoin and one for Dogecoin.
compileG :: BitMLx.G -> (BitML.G BCoins, BitML.G DCoins)
-- | Deposits translate to deposits on the respective blockchains.
-- TODO: when we add the stipulation protocol, these should actually
-- translate to volatiles that will we put on the funding phase.
compileG (BitMLx.Deposit p (v1,v2) (x1,x2)) = (BitML.Deposit p v1 x1, BitML.Deposit p v2 x2)
-- | Collaterals are implemented using regular deposits.
compileG (BitMLx.Collateral p (v1,v2) (x1,x2)) = (BitML.Deposit p v1 x1, BitML.Deposit p v2 x2)
-- | Step secrets are just regular secrets.
-- In facts, the syntactical differentiation between them is specific to this implementation.
compileG (BitMLx.StepSecret p l (bn, bh) (dn, dh)) = (BitML.Secret p bn bh, BitML.Secret p dn dh)

-- | Compile all preconditions for a BitMLx contract
compilePreconditions :: [BitMLx.G] -> ([BitML.G BCoins], [BitML.G DCoins])
compilePreconditions = unzip . map compileG