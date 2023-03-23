{-# LANGUAGE RecordWildCards #-}
module Compiler.PriorityChoice ( compilePriorityChoice )where

import Data.Map.Strict (elems, lookup, Map)
import Prelude hiding (lookup)

import Coins ( Coins, BCoins, DCoins)
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx
import Syntax.Common ( P (pname), Deposit, Time(..), SName(..), NodeLabel )
import {-# SOURCE #-} Compiler.Contract (compileC, compileD)
import Compiler.Common ( CompilationResult, CompilationError, ErrorType (..) )
import Compiler.Settings ( CompilerSettings (..) )


-- | A priority choice between a guarded contract D and a contract C is compiled to a choice between:
-- - Anyone reveals their step secrets on both chains before time t and we execute D synchronously.
-- - After t passes without anyone revealing a step secret on one chain, we "skip", meaning:
--    - If anyone reveals a step secret on the other chain (meaning they didn't skip there)
--      we punish them by splitting their collateral on this chain among everyone else. The honest participants
--      then also receive a refund for their deposits and their collaterals.
--    - If time t' passes and everyone is skipping synchronously, then we execute C.
compilePriorityChoice :: CompilerSettings -> BitMLx.D -> BitMLx.C -> Maybe Time -> Either CompilationError CompilationResult
compilePriorityChoice settings@CompilerSettings{..} d c overrideTimeout =
    do
        (bitcoinD, dogecoinD) <- compileD settings{currentLabel = currentLabel ++ "L", currentTime = currentTime + elapseTime} d
        (bitcoinC, dogecoinC) <- compileC settings{currentLabel = currentLabel ++ "R", currentTime = currentTime + 2 * elapseTime} c
        bitcoinStepSecrets <- eitherLookup currentLabel bitcoinStepSecretsMap (StepSecretsNotFound, "Bitcoin: " ++ currentLabel)
        dogecoinStepSecrets <- eitherLookup currentLabel dogecoinStepSecretsMap (StepSecretsNotFound, "Dogecoin: " ++ currentLabel)
        bitcoinResult <- compilePriorityChoice' bitcoinStepSecrets dogecoinStepSecrets bitcoinBalance bitcoinD bitcoinC
        dogecoinResult <- compilePriorityChoice' dogecoinStepSecrets bitcoinStepSecrets dogecoinBalance dogecoinD dogecoinC
        Right (bitcoinResult, dogecoinResult)
    where
        compilePriorityChoice' :: Coins c =>  Map P SName -> Map P SName -> c -> BitML.C c -> BitML.C c -> Either CompilationError (BitML.C c)
        compilePriorityChoice' activeStepSecrets pasiveStepSecrets balance d' c' = do
            let t = currentTime + elapseTime
                t' = currentTime + 2 * elapseTime
                newSettings =  settings { currentTime = currentTime + 2 * elapseTime }
            punish <- punishAnyone participants balance pasiveStepSecrets
            let skip = tau (punish ++ [BitML.After t' (tau c') | c' /= []])
            Right (revealAny (elems activeStepSecrets) d' ++ [BitML.After t skip])

-- | Small cheat to convert a guarded contract into a contract.
-- Notice that the price of using tau is that it introduces an
-- extra transaction, with it's associated transaction fees.
tau :: BitML.C c -> BitML.D c
tau = BitML.Reveal []

-- | Auxiliary function to convert a lookup from Maybe to Either
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

-- | Auxiliary function to divide some value of coins into many.
-- Because coins are integer values, we return a compilation error
-- if the division is not possible.
divideCoins :: Coins c => c -> Int -> Either CompilationError c
divideCoins coins divisor
  | divisor == 0 = Left (NonDividableCollaterals, "Division by zero")
  | coins `mod` fromIntegral divisor /= 0 = Left (NonDividableCollaterals, "Coins are not divisible by divisor")
  | otherwise = Right (coins `div` fromIntegral divisor)

-- | Punish a participant by splitting it's collateral among all others.
punishOne :: Coins c => P -> [P] -> c -> Map P SName -> Either CompilationError (BitML.D c)
punishOne punishedParticipant honestParticipants balance stepSecrets = do
    secret <- eitherLookup punishedParticipant stepSecrets (StepSecretsNotFound, pname punishedParticipant)
    let executePunishment = BitML.Split (map (\p -> (balance, [BitML.Withdraw p])) honestParticipants)
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
