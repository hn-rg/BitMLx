module Compiler.WellFormed where

import Syntax.BitMLx (TimedPreconditions (TimedPreconditions), Contract (PriorityChoice, Withdraw, WithdrawAll), ContractAdvertisement(ContractAdvertisement), Precondition, GuardedContract (Split, Reveal, RevealIf, WithdrawD, WithdrawAllD, Auth) )
import Syntax.Common (P, Time, SName)
import Coins (BCoins (BCoins), DCoins (DCoins))
import Compiler.Error (CompilationError (..))
import Compiler.Settings (participantsFromPreconditions, secretsFromPreconditions, balanceFromPreconditions)
import Compiler.Auxiliary (assertList, assertCondition)

-- | Parameter record for well formedness check
data WellFormedParameters = WellFormedParameters {
    preconditions :: [Precondition]
    , bcoins :: BCoins
    , dcoins :: DCoins
}

-- | Function checks well formedness of contract advertisement
assertWellFormed :: ContractAdvertisement -> Either CompilationError ()
assertWellFormed advertisement = let
        ContractAdvertisement timedPreconditions contract = advertisement
        TimedPreconditions startTime timeElapse preconditions = timedPreconditions
        bcoins = balanceFromPreconditions fst preconditions
        dcoins = balanceFromPreconditions snd preconditions
    in 
        assertWellFormedContract WellFormedParameters{preconditions = preconditions, bcoins=bcoins, dcoins=dcoins} contract

-- | Well formedness check for non-guarded contracts
assertWellFormedContract :: WellFormedParameters -> Contract-> Either CompilationError ()
assertWellFormedContract wellFormedParameters contract = do
    case contract of
--  For PriorityChoice, we recursively check both child contracts for well formedness
        PriorityChoice leftGuardedContract rightContract ->
                assertList [assertWellFormedContractGuarded wellFormedParameters leftGuardedContract,
                    assertWellFormedContract wellFormedParameters rightContract]
--  For Withdraw(All)D, we check that all available funds are distributed and all participants have their deposit in the preconditions 
        Withdraw fundsMapping ->
            let
                participants =  [participant | (_, participant) <- fundsMapping ]
                (bcoinsSplitted, dcoinsSplitted) = unzip [coins | (coins, _) <- fundsMapping]
            in assertList [assertUserInPreconditions participants preconditions,
                    assertCondition (sum bcoinsSplitted == bcoins && sum dcoinsSplitted == dcoins) InconsistentWithdraw]
        WithdrawAll participant ->
            assertUserInPreconditions [participant] preconditions
    where
        WellFormedParameters preconditions bcoins dcoins = wellFormedParameters


-- | Well formedness check for guarded contracts
assertWellFormedContractGuarded :: WellFormedParameters -> GuardedContract -> Either CompilationError ()
assertWellFormedContractGuarded wellFormedParameters guardedContract = do
    case guardedContract of
--  For Split, we recursively check inner contracts for well formedness and that all available funds are distributed.
        Split contractMapping ->
            let
                (bcoinsSplitted, dcoinsSplitted) = unzip [coins | (coins, _) <- contractMapping]
            in assertList
                ([ assertWellFormedContract wellFormedParameters{bcoins=bcoins, dcoins=dcoins} subContract | ((bcoins, dcoins), subContract) <- contractMapping ]
                ++ [assertCondition (sum bcoinsSplitted == bcoins && sum dcoinsSplitted == dcoins) InconsistentSplit])
--   For Reveal(If), we recursively check inner contracts for well formedness and check that all secrets are commited in the preconditions.
        Reveal secrets contract ->
                assertList [assertWellFormedContract wellFormedParameters contract,
                    assertSecretInPreconditions secrets preconditions]
        RevealIf secrets _ revealContract ->
                assertList [assertWellFormedContract wellFormedParameters revealContract,
                    assertSecretInPreconditions secrets preconditions]
--  For Withdraw(All)D, we check that all available funds are distributed and all participants have their deposit in the preconditions.
        WithdrawD fundsMapping ->
            let
                participants =  [participant | (_, participant) <- fundsMapping ]
                (bcoinsSplitted, dcoinsSplitted) = unzip [coins | (coins, _) <- fundsMapping]
            in assertList [assertUserInPreconditions participants preconditions,
                assertCondition (sum bcoinsSplitted == bcoins && sum dcoinsSplitted == dcoins) InconsistentWithdraw]
        WithdrawAllD participant ->
            assertUserInPreconditions [participant] preconditions
--   For Auth, we recursively check inner contracts for well formedness and check that all participants have their deposit in the preconditions.
        Auth participants guardedContract ->
            assertList [assertWellFormedContractGuarded wellFormedParameters guardedContract,
                assertUserInPreconditions participants preconditions]
    where
        WellFormedParameters preconditions bcoins dcoins = wellFormedParameters

-- | Checks that all participants have deposits in the preconditions.
assertUserInPreconditions :: [P] -> [Precondition] -> Either CompilationError ()
assertUserInPreconditions participants preconditions =
    assertItemsInPreconditions participants preconditions participantsFromPreconditions NoDeposit

-- | Checks that all secrets are committed in the preconditions.
assertSecretInPreconditions :: [SName] -> [Precondition] -> Either CompilationError ()
assertSecretInPreconditions secrets preconditions =
    assertItemsInPreconditions secrets preconditions secretsFromPreconditions UncommitedSecret

-- | Auxiliary function to provide membership test on preconditions.
assertItemsInPreconditions :: (Eq a) =>  [a] -> [Precondition] -> ([Precondition] -> [a]) -> (a -> CompilationError) -> Either CompilationError ()
assertItemsInPreconditions items preconditions selector error =
    assertList [assertCondition (s `elem` selector preconditions) (error s) | s <- items ]

