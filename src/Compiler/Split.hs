{-# LANGUAGE RecordWildCards #-}
module Compiler.Split ( SplitBranch, compileSplit ) where

import Data.Map (elems)
import Data.Ratio (numerator, denominator)

import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import {-# SOURCE #-} Compiler.Contract (compileC)
import Coins (BCoins, DCoins, Coins)
import Compiler.Error (CompilationError(..))
import Compiler.Settings (CompilerSettings (..))
import Compiler.Auxiliary (eitherLookup, revealAny, listEither, enumerate)
import Syntax.BitML (GuardedContract(Split))


-- | A Split statement in BitMLx is also compiled to a split in BitML between the
-- compiled subcontracts.
-- Keep in mind that while BitML uses absolute values for specifying the subcontract
-- new balances, BitMLx uses proportions, in this case represented as Rational numbers. 
-- This is because, the balance of the compiled BitML contract will actually be the sum
-- of the BitMLx balance and the collateral deposits. When splitting the balance,
-- we also need to split the collateral using the same proportion.
--
-- We additionally perform a sanity check that proportions (on thsi chain)
-- are on the 0-1 range and add up to 1.

type SplitBranch = ((BCoins, DCoins), BitMLx.Contract)

compileSplit :: Coins c => CompilerSettings c -> [SplitBranch] -> Either CompilationError (BitML.Contract c)
compileSplit settings branches = do
    let label = currentLabel settings
    stepSecrets <- eitherLookup label (stepSecretsByLabel settings) (StepSecretsNotFoundForNode label)
    compiledBranches <- listEither [
        compileSplitBranch settings i branch
        | (i, branch) <- enumerate branches
        ]
    Right $ revealAny (elems stepSecrets) [Split compiledBranches]


compileSplitBranch :: Coins c => CompilerSettings c -> Integer -> SplitBranch -> Either CompilationError (c, BitML.Contract c)
compileSplitBranch settings index branch = do
    let ((btcBalance, dogeBalance), subcontract) = branch
        nParticipants = fromInteger (toInteger (length (participants settings)))
        balance = coinChooser settings (btcBalance, dogeBalance)
        collateral = (nParticipants - 2) * balance
        totalFunds = balance + nParticipants * collateral
        (choiceLabel, splitLabel) = currentLabel settings
        newLabel = (choiceLabel, splitLabel ++ show index)
        newSettings = settings{balance=balance, collateral=collateral, totalFunds=totalFunds, currentLabel=newLabel}
    compiledSubcontract <- compileC newSettings subcontract
    Right (totalFunds, compiledSubcontract)