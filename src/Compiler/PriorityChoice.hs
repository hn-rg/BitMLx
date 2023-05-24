{-# LANGUAGE RecordWildCards #-}
module Compiler.PriorityChoice ( compilePriorityChoice )where

import Data.Map.Strict (elems, lookup, Map)
import Prelude hiding (lookup)

import Coins ( Coins, BCoins, DCoins)
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Syntax.Common ( P (pname), Deposit, Time(..), SName(..), NodeLabel )
import {-# SOURCE #-} Compiler.Contract (compileC, compileD)
import Compiler.Error ( CompilationError(..) )
import Compiler.Settings ( CompilerSettings (..) )


-- | A priority choice between a guarded contract D and a contract C is compiled to a choice between:
-- - Anyone reveals their step secrets on both chains before time t and we execute D synchronously.
-- - After t passes without anyone revealing a step secret on one chain, we "skip", meaning:
--    - If anyone reveals a step secret on the other chain (meaning they didn't skip there)
--      we punish them by splitting their collateral on this chain among everyone else. The honest participants
--      then also receive a refund for their deposits and their collaterals.
--    - If time t' passes and everyone is skipping synchronously, then we execute C.
compilePriorityChoice :: Coins c => CompilerSettings c -> BitMLx.D -> BitMLx.C -> Maybe Time -> Either CompilationError (BitML.C c)
compilePriorityChoice settings@CompilerSettings{currentLabel = (choiceLabel, splitLabel), ..} d c overrideTimeout = do
        d' <- compileD settings{currentLabel = (choiceLabel ++ "L", splitLabel), currentTime = currentTime + elapseTime} d
        c' <- compileC settings{currentLabel = (choiceLabel ++ "R", splitLabel), currentTime = currentTime + 2 * elapseTime} c
        thisChainStepSecrets <- eitherLookup (choiceLabel, splitLabel) thisChainStepSecretsByLabel (StepSecretsNotFoundForNode (choiceLabel, splitLabel))
        otherChainStepSecrets <- eitherLookup (choiceLabel, splitLabel) otherChainStepSecretsByLabel (StepSecretsNotFoundForNode (choiceLabel, splitLabel))
        punish <- punishAnyone participants balance otherChainStepSecrets
        let t = currentTime + elapseTime
            t' = currentTime + 2 * elapseTime
            skip = tau (punish ++ [BitML.After t' (tau c') | c' /= []])
        Right (revealAny (elems thisChainStepSecrets) [d'] ++ [BitML.After t skip])

-- | Small cheat to convert a guarded contract into a contract.
-- Notice that the price of using tau is that it introduces an
-- extra transaction, with it's associated transaction fees.
tau :: BitML.C c -> BitML.D c
tau = BitML.Reveal []

-- | Auxiliary function to convert a lookup from Maybe to Either
-- takes as argument the error to throw if the lookup
-- returns nothing.
eitherLookup :: Ord k => k -> Map k v -> e -> Either e v
eitherLookup k m e = case lookup k m of
    Just v -> Right v
    Nothing -> Left e

-- | Auxiliary function to either get all right results or
-- short-circuit on the first error.
sequenceEither :: [Either e a] -> Either e [a]
sequenceEither [] = Right []
sequenceEither (Left e : _) = Left e
sequenceEither (Right x : xs) =
    case sequenceEither xs of
        Left e -> Left e
        Right ys -> Right (x : ys)

-- | An alternative reveal construction that work by requiring any of a list of secrets
-- to be revealed instead of all of them.
revealAny :: Coins c =>  [SName] -> BitML.C c -> BitML.C c
revealAny secrets continuation = map (\s -> BitML.Reveal [s] continuation) secrets

-- | Punish a participant by splitting it's collateral among all others.
punishOne :: Coins c => P -> [P] -> c -> Map P SName -> Either CompilationError (BitML.D c)
punishOne punishedParticipant honestParticipants balance stepSecrets = do
    secret <- eitherLookup punishedParticipant stepSecrets (StepSecretsNotFoundForParticipant punishedParticipant)
    let executePunishment = if length honestParticipants > 1
        then
            BitML.Split (map (\p -> (balance, [BitML.Withdraw p])) honestParticipants)
        else
            -- Optimization: Avoid having a single branch split.
            BitML.Withdraw (head honestParticipants)
    Right (BitML.Reveal [secret] [executePunishment])

-- | Given a list tuples (p, c, s) where p is the participant, c  is the value of their collateral
-- and s is one of their secrets, creates a contract that punishes any participant for
-- revealing their secret, by splitting their collateral among the rest.
punishAnyone :: Coins c => [P] -> c -> Map P SName -> Either CompilationError (BitML.C c)
punishAnyone participants balance stepSecrets =
    sequenceEither [
        punishOne p (filter (/= p) participants) balance stepSecrets
        | p <- participants
    ]
