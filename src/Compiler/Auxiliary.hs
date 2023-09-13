module Compiler.Auxiliary where

import Prelude hiding (lookup)
import Data.Map.Strict (elems, lookup, Map)

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