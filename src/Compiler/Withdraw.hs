{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Compiler.Withdraw where

import Coins ( DCoins, BCoins, Coins )
import Syntax.Common ( P, Deposit )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Compiler.Settings ( CompilerSettings(..) )
import qualified Data.Map.Strict as Map
import Data.List (delete)

-- | A withdraw statement in BitMLx is compiled in BitML as a split between:
-- - The corresponding withdraw statement for the specified participant.
-- - For each participant, a withdraw of their collateral.
compileWithdraw :: Coins c => CompilerSettings c -> P -> BitML.D c
compileWithdraw CompilerSettings{..} p =
    BitML.Split (
            (balance + collateral, [BitML.Withdraw p])
            : map (\p' -> (collateral, [BitML.Withdraw p'])) others
        )
    where others = delete p participants
