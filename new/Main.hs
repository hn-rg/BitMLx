module Main where

import Syntax
import Examples
import Trifunctor

import Data.Char
import Data.List
import GHC.TypeLits

import Control.Monad.ST
import Data.Function
import Data.Bifunctor as DB

import qualified Data.Vector as V

{- _____ Goal _____

        BitMLx ---> / preprocessing / ---> / compilation / ---> BitML||
   ________________
-}

tCheat = 100
tInit = 1
level = 1


compileC :: Cx -> Vb -> Vd -> Vb -> Vd -> Int -> Int 
        -> [Pname] -> Level -> V.Vector (Sname,Pname) 
        -> V.Vector (Sname,Pname) -> Time 
        -> (C, C)
compileC (Withdrawx p : [] ) ub ud ubCol udCol n m ps i s1 s2 t = ( [Withdraw p], [Withdraw p] )

compileC ( Withdrawx p : d ) ub ud ubCol udCol n m ps i s1 s2 t = 
        let         
            d'  = Split ( [ub] ++ [ubCol `div` n | i <- [1..n] ] ) ( [Withdraw p] : [ [Withdraw i] | i <- ps] )   -- bitcoin  
            dx' = Split ( [ud] ++ [udCol `div` n | i <- [1..n] ] ) ( [Withdraw p] : [ [Withdraw i] | i <- ps]  )  -- dogecoin
          
            -- | this is the first big choice of the compiled contract, where everything goes as expected, 2 remainning oooof
            d1  = concatChoices [d'] s1 n m i 1                  -- bitcoin
            d1x = concatChoices [dx'] s2 n m i 1                -- dogecoin
            
            d2  = cheatCase ps n ub ubCol s2 i m 1 t
            d2x = cheatCase ps n ud udCol s1 i m 1 t

            
        in (d1++d2,d1x++d2x)


-- | i is level counter, j is participant counter (IMPORTANT: INIT VALUE -> 1)
-- n is the number of participants, m is the number of prichoices

-- | this function takes as input a contract and a vector of secrets and creates a new contract
-- which consist of as many guarded contracts as the number of participants
-- each participant can stipulate the initial contract, providing her secret
concatChoices :: C -> V.Vector (Sname,Pname) -> Int -> Int -> Int -> Int -> C
concatChoices d s n m i j 
        | j <= n    = Reveal [fst $ s V.! (i - 1 + m * (j - 1) ) ] d : concatChoices d s n m i (j + 1)
        | otherwise = []        


-- | i is level counter, j is participant counter
-- n is the number of participants, m is the number of prichoices
cheatCase ::  [Pname] -> Int -> Int -> Int -> V.Vector (Sname,Pname) -> Level -> Int -> Int -> Time -> C
cheatCase ps n u ucol s i m j t
        | j <= n   = k : cheatCase ps n u ucol s i m (j + 1) t
        |otherwise = []
   where k = After t (Reveal [fst $ s V.! p] 
                        [ Split  [ ucol `div` (n-1) | i <-  ps' ]        -- add deposit of Pi
                         [ [Withdraw i] | (_,i) <- ps' ] ] )   -- see *** below
         p = i - 1 + m * (j - 1)
         ps' = vecCompr p s
-- | *** participants list should be extracted from lSecrets so as the order of appearance is the same in both.

dep :: [(Pname, Int)] -> Pname -> Int
dep ((x, u) : ps) y 
        | x == y = u
        | otherwise = dep ps y

vecCompr :: Int -> V.Vector (Sname,Pname) -> [(Sname,Pname)]
vecCompr p s = let v = V.take p s V.++ V.drop (p+1) s 
                in V.toList v

-- | well - formness of collaterals and extra secrets
{- | ________
  1. EACH participant commits to m extra secrets 
        / m = # PriChoice
  2. EACH participant gives an extra deposit of value (n-1) * u 
        / n = # participants, u = initial balance of the contract
-}

check :: Gxl -> Int -> Int -> Vb -> Vd -> [Pname] -> Bool
check g n m u1 u2 p =

        let (b,x) = aux True g n m u1 u2 [] [] []
        in  b && (^&&&) ( trimap (sameElems p) (sameElems p ) (sameElems p) x )
        
        where 
        aux :: Bool -> Gxl -> Int -> Int -> Vb -> Vd -> [Pname] -> [Pname] -> [Pname] -> ( Bool, ([Pname], [Pname], [Pname]) )
        aux False _                              _ _ _ _   p1 p2 p3 = ( False, ( p1, p2, p3 ) )
        aux True (SecretPlusB p s : xs)          n m u1 u2 p1 p2 p3 = let b = length s == m 
                                                                      in aux b xs n m u1 u2 (p : p1) p2 p3
        aux True (SecretPlusD p s : xs)          n m u1 u2 p1 p2 p3 = let b = length s == m 
                                                                      in aux b xs n m u1 u2 p1 (p : p2) p3
        aux True (DepCol p (u1,u2) (x1,x2) : xs) n m ub ud p1 p2 p3 = let b = u1 == (n-1) * ub && u2 == (n-1) * ud
                                                                      in  aux b xs n m u1 u2 p1 p2 (p : p3)
        aux True (_ : xs)                        n m u1 u2 p1 p2 p3 = aux True xs n m u1 u2 p1 p2 p3
        aux True []                              n m u1 u2 p1 p2 p3 = ( True, ( p1, p2, p3 ) )



-- | balance of the contract
-- mind that we have one for bitcoin, one for dogecoin
-- so the balance is a tuple of balances, for now
balCol :: Gxl -> Vb -> Vd -> Vb -> Vd -> [(Pname, Vb)] -> [(Pname, Vd)] 
        -> (Vb, Vd, Vb, Vd, [(Pname, Vb)], [(Pname, Vd)])
balCol []                            n1 n2 col1 col2 p1 p2 = (n1, n2, col1, col2, p1, p2)
balCol (Depx p (vb,vd) (_,_) : xs)   n1 n2 col1 col2 p1 p2 = balCol xs (n1 + vb) (n2 + vd) col1 col2 ((p,vb):p1) ((p,vd):p2)
balCol (DepCol p (vb,vd) (_,_) : xs) n1 n2 col1 col2 p1 p2 = balCol xs n1 n2 (col1 + vb) (col2 + vd) p1 p2
balCol ( _ : xs)                     n1 n2 col1 col2 p1 p2 = balCol xs n1 n2 col1 col2 p1 p2


-- | this function takes as input a G describing contract preconditions
-- and outputs 2 vectors of extra secrets as describind in the following lines
-- each vector has length n * m, where n the number of participants and m the number of priority choices
-- indexing the vector as  i * m / 0 <= i <= n - 1, gives as the first secret commited (the secret for the first level) by each user
lSecrets :: Gxl -> V.Vector (Sname, Pname) -> V.Vector (Sname, Pname) -> (V.Vector (Sname, Pname), V.Vector (Sname, Pname))
lSecrets []                       v1 v2 = (v1, v2)      
lSecrets ( SecretPlusB p s : xs ) v1 v2 = let s' = map (updateTuple2 p) s in lSecrets xs (V.fromList s' V.++ v1) v2
lSecrets ( SecretPlusD p s : xs ) v1 v2 = let s' = map (updateTuple2 p) s in lSecrets xs v1 (V.fromList s' V.++ v2)
lSecrets ( _ : xs )               v1 v2 = lSecrets xs v1 v2   

-- dep :: Gxl -> Vb -> Vd -> (V.Vector (Pname,Vb), V.Vector)
main :: IO ()
main = do
        let 
            v = V.empty :: V.Vector (Sname,Pname)                       -- init empty vector
            n = length p1                        -- number of participants
            (u1, u2, col1, col2, dep1, dep2) = balCol g1 0 0 0 0 [] []           -- balance of contract + collaterals
            m = length cSimpleTest - 1           -- number of priority choices
            p = map (\ (Par x y) -> x) p1        -- list of participants' names
            (s1,s2) = lSecrets g1 v v            -- list (vector) of secrets' names
            t = check g1 n m u1 u2 p             -- check if contract preconditions are well defined
            c' = compileC cSimpleTest u1 u2 col1 col2 n m p level s1 s2 tInit  -- compile contract
        {-print c1
        print g1
        print (u1, u2)
        print (col1, col2)
        print m 
        print p
        print (s1, s2)
        print t
        -}

        when t (print $ fst c')
        when t (print $ snd c')

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

updateTuple2 :: c -> (a,b) -> (a,c)
updateTuple2 z (x,y)  = (x,z)

-- | check https://hackage.haskell.org/package/tuple-0.2.0.1/docs/src/Data-Tuple-Select.html#sel1
-- and https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Tuple.html
fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,y,_) = y

thd :: (a,b,c) -> c
thd (_,_,z) = z