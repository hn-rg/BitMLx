
module Trifunctor(
    Trifunctor(..),
) where

import Control.Monad(liftM3)

-- | Extends the 'Functor' concept to three type arguments.  (Note: we
-- are missing analogies to the functions 'first' and 'second' found
-- in "Data.Bifunctor".)
class Trifunctor f where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> (f a b c -> f a' b' c')

instance Trifunctor (,,) where
  trimap f g h (x,y,z) = (f x, g y, h z)


-- | Extends the 'Traversable' concept to three type arguments.
-- (Note: we are missing analogies to the functions 'bitraverse',
-- 'bisequenceA' and 'bimapM' found in "Data.Bitraversable".)
class Tritraversable t where

    -- | Sequences all the actions in the structure, building a new
    -- structure with the same shape using the results of the actions.
    trisequence :: Monad m => t (m a) (m b) (m c) -> m (t a b c)

instance Tritraversable (,,) where
    trisequence (ma, mb, mc) = liftM3 (,,) ma mb mc

