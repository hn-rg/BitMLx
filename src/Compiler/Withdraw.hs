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
compileWithdraw :: CompilerSettings -> P -> (BitML.C BCoins, BitML.C DCoins)
compileWithdraw CompilerSettings{..} p =
    let
        others = delete p participants
        compileWithdraw' :: Coins c => c -> c -> BitML.C c
        compileWithdraw' balance collateral =  [
                BitML.Split (
                    (balance + collateral, [BitML.Withdraw p])
                    : map (\p' -> (collateral, [BitML.Withdraw p'])) others
                )
            ]
    in (compileWithdraw' bitcoinBalance bitcoinCollateral, compileWithdraw' dogecoinBalance dogecoinCollateral)