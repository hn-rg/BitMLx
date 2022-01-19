module Auxiliary where

--import Data.Char
import Data.List hiding (sort)

import Data.Function

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
-- auxiliary stuff

when :: MonadFail m => Bool -> m a -> m a
when True p  = p
when False _ = fail "when failed"

(^+) :: Num a => (a,a) -> (a,a) -> (a,a)
(x1,x2) ^+ (y1,y2) = (x1+y1, x2+y2)

-- | checks if two lists have the same elements, 
-- even if they do not appear in the same order
sameElems :: (Eq a) => [a] -> [a] -> Bool
sameElems x y = null (x \\ y) && null (y \\ x)

(^&&) :: (Bool, Bool) -> Bool
(^&&) (x,y) = x && y

(^&&&) :: (Bool, Bool, Bool) -> Bool
(^&&&) (x,y,z) = x && y && z

updateTuple2 :: c -> (a,b) -> (c,a)
updateTuple2 z (x,y)  = (z,x)

-- | check https://hackage.haskell.org/package/tuple-0.2.0.1/docs/src/Data-Tuple-Select.html#sel1
-- and https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Tuple.html
fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,y,_) = y

thd :: (a,b,c) -> c
thd (_,_,z) = z

sortVec :: Ord a => V.Vector (a,b) -> V.Vector (a,b)
sortVec = V.modify $ I.sortBy ( compare `on` fst )

sortList :: Ord a => [a] -> [a]
sortList = V.toList . V.modify I.sort . V.fromList

