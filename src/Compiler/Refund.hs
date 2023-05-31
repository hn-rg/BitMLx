{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Refund where

import Data.Map.Strict (lookup)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

import Syntax.Common (P)
import Syntax.BitML ( D(Split, Withdraw) )
import qualified Syntax.BitML as BitML
import Compiler.Error (CompilationError (NoDeposit))
import Compiler.Settings (CompilerSettings (..))
import Coins (Coins)
import Compiler.Auxiliary (sequenceEither, eitherLookup, scaleCoins)

compileRefund :: Coins c => CompilerSettings c -> Either CompilationError (BitML.C c)
compileRefund settings@CompilerSettings{participants} = do
    withdraws <- sequenceEither [ refundForParticipant settings p | p <- participants] 
    Right [Split withdraws]

refundForParticipant :: Coins c => CompilerSettings c -> P -> Either CompilationError (c, BitML.C c)
refundForParticipant settings@CompilerSettings{totalFunds, refundProportionsMap, ..} p = do
    proportion <- eitherLookup p refundProportionsMap (NoDeposit p)
    refundAmount <- scaleCoins totalFunds proportion
    Right (refundAmount, [Withdraw p])
