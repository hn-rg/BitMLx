module Compiler.Auxiliary where

import Prelude hiding (lookup)
import Data.Map.Strict (elems, lookup, Map)
import Data.Ratio (numerator, denominator)

import Coins (Coins)
import Syntax.Common (SName, P)
import qualified Syntax.BitML as BitML
import qualified Syntax.BitMLx as BitMLx

-- | Small cheat to convert a guarded contract into a contract.
-- Notice that the price of using tau is that it introduces an
-- extra transaction, with it's associated transaction fees.
tau :: BitML.Contract c -> BitML.GuardedContract c
tau = BitML.Reveal []

-- | An alternative reveal construction that works by requiring any of a list of secrets
-- to be revealed instead of all of them.
revealAny :: Coins c =>  [SName] -> BitML.Contract c -> BitML.Contract c
revealAny secrets subcontract =  [BitML.Reveal [s] subcontract | s <- secrets]

-- | Auxiliary function to convert a lookup from Maybe to Either
-- takes as argument the error to throw if the lookup
-- returns nothing.
eitherLookup :: Ord k => k -> Map k v -> e -> Either e v
eitherLookup k m e = case lookup k m of
    Just v -> Right v
    Nothing -> Left e

-- | Auxiliary function to either get all right results or
-- short-circuit on the first error.
listEither :: [Either e a] -> Either e [a]
listEither [] = Right []
listEither (Left e : _) = Left e
listEither (Right x : xs) =
    case listEither xs of
        Left e -> Left e
        Right ys -> Right (x : ys)

-- | Auxiliary function to check for existance of an error.
assertList :: [Either e ()] -> Either e ()
assertList [] = Right ()
assertList (Left e : _) = Left e
assertList (_ : xs) = assertList xs

-- | Auxiliary function that returns error when condition is not met.
assertCondition :: Bool -> e -> Either e ()
assertCondition bool error =
    if bool then Right () else Left error


tupleEither :: (Either e a, b) -> Either e (a, b)
tupleEither (Left err, y) = Left err
tupleEither (Right x, y) = Right (x, y)


-- | Creates a list with the elements of the original list enumerated in tuples.
enumerate :: [b] -> [(Integer, b)]
enumerate = zip [0..]