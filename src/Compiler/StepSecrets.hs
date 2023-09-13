{-|
Module      : Compiler.StepSecrets
Description : Step secrets generation

Step secrets are the crucial part of the BItMLx compilation.

Each node on the BitMLx syntax tree is identified with a unique label,
and each participant will commit on both BitML contracts a secret
for each label. This secrets serves as proof that a participant
is actively taking a left choice on a particular execution step.

The functions here traverse the tree and compile a map of all
step secret, conveniently indexed by node label and participant.
-}
module Compiler.StepSecrets (generateStepSecretsMap) where

import qualified Data.Map.Strict as Map

import Coins (Coins)
import Syntax.Common (NodeLabel, P (pname), SName, emptyLabel)
import Syntax.BitMLx (Contract(PriorityChoice, Withdraw, WithdrawAll), GuardedContract (WithdrawD, Split, WithdrawAllD, Auth, Reveal, RevealIf))
import qualified Syntax.BitMLx as BitMLx
import qualified Syntax.BitML as BitML
import qualified Compiler.Auxiliary as Auxiliary
import Compiler.Auxiliary (enumerate)

-- | Haskell magic to convert the step secrets to a format easy to index first by
-- node and then by participant.
-- The heavy-lifting is done b 
generateStepSecretsMap :: [P] -> Either BitMLx.Contract BitMLx.GuardedContract -> Map.Map NodeLabel (Map.Map P SName)
generateStepSecretsMap participants contract =
    let stepSecretTuples = case contract of
            Left c -> stepSecretsC participants emptyLabel c
            Right d -> stepSecretsD participants emptyLabel d
    in Map.fromListWith
        (Map.unionWith (++))
        [(l, Map.singleton p s) | (l, byParticipant) <- stepSecretTuples, (p, s) <- byParticipant]

-- | Generate a list of all needed step secrets for a contract.
-- Refer to module documentation for explanation of why this is needed.
stepSecretsC :: [P] -> NodeLabel -> BitMLx.Contract -> [(NodeLabel, [(P, SName)])]
stepSecretsC participants currentLabel@(moves, splits) contract =
    case contract of
        (PriorityChoice d c) ->
            stepSecretsD participants (moves ++ "L", splits) d
            ++ stepSecretsC participants (moves ++ "R", splits) c
        (Withdraw _) -> []
        (WithdrawAll _) -> []

-- | Generate a list of all needed step secrets for a contract.
-- Refer to module documentation for explanation of why this is needed.
stepSecretsD :: [P] -> NodeLabel -> BitMLx.GuardedContract -> [(NodeLabel, [(P, SName)])]
stepSecretsD participants currentLabel@(moves, splits) guardedContract =
    case guardedContract of
        (WithdrawD _) -> [onThisNode]
        (WithdrawAllD _) -> [onThisNode]
        (Split branches) -> onThisNode : onAllBranches branches
        (Auth signers guardedContract) -> onFollowUpGuardedContract guardedContract
        (Reveal _ contract) -> onThisNode : onFollowUpContract contract
        (RevealIf _ _ contract) -> onThisNode : onFollowUpContract contract
    where
        onThisNode = (currentLabel, [(p, stepSecretName currentLabel p) | p <- participants])
        onAllBranches bs = concat [
            stepSecretsC participants (moves, splits ++ show index) branch
            | (index, (_value, branch)) <- enumerate bs
            ]
        onFollowUpContract c = stepSecretsC participants currentLabel c
        onFollowUpGuardedContract d = stepSecretsD participants currentLabel d

-- | Generate a unique name for each step secret.
stepSecretName :: NodeLabel -> P -> String
stepSecretName currentLabel@(moves, splits) participant =
    "StepSecret_"
    ++ pname participant
    ++ "__" ++ moves
    ++ "_" ++ splits
