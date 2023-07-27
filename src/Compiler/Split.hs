{-# LANGUAGE RecordWildCards #-}
module Compiler.Split ( compileSplit ) where

import Data.Map (elems)
import Data.Ratio (numerator, denominator)

import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import {-# SOURCE #-} Compiler.Contract (compileC)
import Coins (BCoins, DCoins, Coins)
import Compiler.Error (CompilationError(..))
import Compiler.Settings (CompilerSettings (..))
import Compiler.Auxiliary (eitherLookup, revealAny, listEither, scaleCoins, enumerate)
import Syntax.BitML (D(Split))


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
compileSplit :: Coins c => CompilerSettings c -> [((Rational, Rational), BitMLx.C)] -> Either CompilationError (BitML.C c)
compileSplit settings@CompilerSettings{currentLabel = (choiceLabel, splitLabel), ..} subcontractsWithProportions = do
    thisChainStepSecrets <- eitherLookup (choiceLabel, splitLabel) stepSecretsByLabel (StepSecretsNotFoundForNode (choiceLabel, splitLabel))
    let coinProportions = map getProportionOnThisCoin subcontractsWithProportions
    if sum coinProportions /= 1 || any (\p -> p < 0 || p > 1) coinProportions
        then Left (InconsistentSplit  coinProportions)
        else Right ()
    compiledSubcontracts <- listEither (
        map compileSubcontract (enumerate subcontractsWithProportions)
        )
    Right $ revealAny (elems thisChainStepSecrets) [Split compiledSubcontracts]

    where
        -- | This function is basically `coinChooser.fst`. But we define it as an auxiliary function
        -- just to name the arguments, hopefully increasing readability.
        getProportionOnThisCoin :: ((Rational, Rational), BitMLx.C) -> Rational
        getProportionOnThisCoin a@((_bitcoinProportions, _dogecoinProportions), _subcontract) = (coinChooser.fst) a
        compileSubcontract (index, (proportions, subcontract)) = do
            let coinProportion = coinChooser proportions
                newLabel = (choiceLabel, splitLabel ++ show index)
            newBalance <- balance `scaleCoins` coinProportion
            newCollateral <- collateral `scaleCoins` coinProportion
            newTotalFunds <- totalFunds `scaleCoins` coinProportion
            compiledSubcontract <- compileC settings{balance=newBalance, collateral=newCollateral, totalFunds=newTotalFunds, currentLabel=newLabel} subcontract
            Right (newTotalFunds, compiledSubcontract)