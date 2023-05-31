module Compiler.Auxiliary where

import Prelude hiding (lookup)
import Data.Map.Strict (elems, lookup, Map)
import Data.Ratio (numerator, denominator)

import Coins (Coins)
import Syntax.Common (SName)
import Syntax.BitML (D (Reveal), C)
import Compiler.Error (CompilationError (NonDividableCoins))


-- | Small cheat to convert a guarded contract into a contract.
-- Notice that the price of using tau is that it introduces an
-- extra transaction, with it's associated transaction fees.
tau ::C c -> D c
tau = Reveal []

-- | An alternative reveal construction that works by requiring any of a list of secrets
-- to be revealed instead of all of them.
revealAny :: Coins c =>  [SName] -> C c -> C c
revealAny secrets subcontract =  [Reveal [s] subcontract | s <- secrets]

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

-- Auxiliary function to split funds by some ratio.
-- Fails if the result would be a non-whole number.
scaleCoins :: Coins c => c -> Rational -> Either CompilationError c
scaleCoins coins r
    | (c * numerator r) `mod` denominator r == 0 = Right (fromInteger (c * numerator r `div` denominator r))
    | otherwise = Left $ NonDividableCoins c r
    where c = toInteger coins