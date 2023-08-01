module Compiler.Error (CompilationError(..)) where

import Data.Map.Strict (Map, empty)

import Coins ( Coins, BCoins, DCoins)
import Syntax.Common ( Time, SName, P, NodeLabel )
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx

-- | Different kind of errors that the compiler can output 
data CompilationError =
    -- | Error attempting to divide coin quantities.
    -- The `Integer` parameter will always come from some `Coins a` type.
    -- The reason we don't use a `Coins a` type here is it messes with the
    -- `Show` derivation and we would have to write a instance manually.
    NonDividableCoins Integer Rational
    -- Split proportions don't add up to 1
    | InconsistentSplit [Rational]
    -- | No step secrets for node.
    | StepSecretsNotFoundForNode NodeLabel
    -- | No step secrets for participant.
    | StepSecretsNotFoundForParticipant P
    -- | Not implemented yet.
    | NotImplemented
    -- | This Participant has no deposit
    | NoDeposit P
    | UnsafeTimedPriorityChoice Time
    deriving (Show, Eq)
