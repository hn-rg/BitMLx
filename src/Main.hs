{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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

-- | vector of secrets should include except for the secrets' names 
-- also the participants' names !!

-- | fixed i guess
{-
        ooooooooofffff I 've made a mistake I guess
        participants should commit different secrets for different blockchains
        check syntax !!!
-}



{- _____ Goal _____

        BitMLx ---> / preprocessing / ---> / compilation / ---> BitML||
   ________________
-}


-- | well - formness of collaterals and extra secrets
{- ________
  1. EACH participant commits to m extra secrets 
        / m = # PriChoice
  2. EACH participant gives an extra deposit of value (n-1) * u 
        / n = # participants, u = initial balance of the contract
-}
tCheat = 100

check :: Gx -> Int -> Int -> Vb -> Vd -> [Pname] -> Bool
check g n m u1 u2 p =
        let x = checkSec g m [] []
            y = checkCol g n u1 u2 []
        in   (^&&&) ( trimap (id) (sameElems p ) (sameElems p) x) && ( (^&&) $ DB.bimap (id) (sameElems p) y )
        
        where 
        checkSec :: Gx -> Int -> [Pname] -> [Pname] -> (Bool, [Pname], [Pname])
        checkSec (SecGx gx1 gx2) m ps ps'   =  let b1 = checkSec gx1 m [] []
                                                   b2 = checkSec gx2 m ps ps'
                                               in trimap (fst' b1 &&) (snd' b1 ++) (thd b1 ++) b2                   -- trimap f g h (x,y,z) = (f x, g y, h z)
        checkSec (SecretPlusB p xs) m ps ps' =   (length xs == m, p:ps, ps')
        checkSec (SecretPlusD p xs) m ps ps' =   (length xs == m, ps, p:ps')
        checkSec _ _ ps ps'                  =   (True, ps, ps')

        checkCol :: Gx -> Int -> Vb -> Vd -> [Pname] -> (Bool, [Pname])
        checkCol (SecGx gx1 gx2) n u1 u2 ps             =  let b1 = checkCol gx1 n u1 u2 []
                                                               b2 = checkCol gx2 n u1 u2 ps
                                                           in DB.bimap (fst b1 &&) (snd b1 ++) b2                       
        checkCol (DepCol p (u1,u2) (x1,x2) ) n ub ud ps =  (u1 == (n-1) * ub && u2 == (n-1) * ud , p:ps) 
        checkCol _ _ _ _ ps                             = (True, ps)                                     


compileC :: Cx -> Vb -> Vd -> Vb -> Vd -> Int -> Int -> [Pname] -> Level -> V.Vector (Sname,Pname) -> V.Vector (Sname,Pname) -> (C, C)
compileC (PriChoice (Solox (Withdrawx p)) d) ub ud ubCol udCol n m ps i s1 s2 = 
        let         
            -- | here should be UbCol/n but I get a typecheck error bc Int is not instance of Fractional, will check on that later
            d'  = Solo (Split ([ub]++[ubCol | i <- [1..n]]) ([Solo (Withdraw p)] ++ [Solo (Withdraw i) | i <- ps]) )    -- bitcoin  
            dx' = Solo (Split ([ud]++[udCol | i <- [1..n]]) ([Solo (Withdraw p)] ++ [Solo (Withdraw i) | i <- ps]) )    -- dogecoin
            
            d   = Solo ( Reveal ( [fst $ s1 V.! (i-1)] ) d' )            -- bitcoin
            dx  = Solo ( Reveal ( [fst $ s2 V.! (i-1)] ) dx' )           -- dogecoin
            
            -- | this is the first big choice of the compiled contract, where everything goes as expected, 2 remainning oooof
            d1  = concatChoices d' s1 n m i 2 d                  -- bitcoin
            d1x = concatChoices dx' s2 n m i 2 dx                -- dogecoin

            
        in (d1,d1x)
compileC  (Solox (Withdrawx p) ) ub ud ubCol udCol n m ps i s1 s2 = (Solo (Withdraw p), Solo (Withdraw p))


-- | i is level counter
-- j is participant counter
-- n is the number of participants,
-- m is the number of prichoices
cheatCase :: [Pname] -> Int -> Int -> V.Vector (Sname,Pname) -> Level -> Int -> Int ->  C
cheatCase ps u ucol s i m j = Solo $ Reveal [fst $ s V.! ( i-1 + m * (j-1) ) ] $ Solo $ Withdraw "A"

-- | i is level counter
-- j is participant counter
-- n is the number of participants,
-- m is the number of prichoices

-- | this function takes as input a contract and a vector of secrets
-- and creates a new contract
-- which consist of as many guarded contracts as the number of secrets
-- each participant can stipulate the initial contract 
-- providing her secret
concatChoices :: C -> V.Vector (Sname,Pname) -> Int -> Int -> Int -> Int -> C -> C
concatChoices d s n m i j prev 
        | j < n     = Choice prev (concatChoices d s n m i (j+1) prev')
        | otherwise = prev'        
        where prev' = Choice prev (Solo (Reveal  ( [fst $ s V.! (i-1 + m * (j-1))] ) d ) ) 

-- | number of participants in the contract
nPar ::  Gx -> Int -> Int
nPar (SecGx gx1 gx2)       n = nPar gx1 n + nPar gx2 n
nPar (Depx _ (_,_) (_,_) ) n = n+1
nPar _                     n = n

-- | list with the names of all participants in the contract
lPar ::  Gx -> [Pname] -> [Pname]
lPar (SecGx gx1 gx2)       ps = lPar gx1 [] ++ lPar gx2 ps
lPar (Depx p (_,_) (_,_) ) ps = p:ps
lPar _                     ps = ps

-- | balance of the contract
-- mind that we have one for bitcoin, one for dogecoin
-- so the balance is a tuple of balances, for now
balance :: Gx -> (Vb,Vd) -> (Vb,Vd)
balance (SecGx gx1 gx2)         (n1,n2) = balance gx1 (n1,n2) ^+ balance gx2 (n1,n2)
balance (Depx _ (vb,vd) (_,_) ) (n1,n2) = (n1+vb, n2+vd)
balance _                       (n1,n2) = (n1,n2)

collateral :: Gx -> (Vb,Vd) -> (Vb,Vd)
collateral (SecGx gx1 gx2)           (n1,n2) = collateral gx1 (n1,n2) ^+ collateral gx2 (n1,n2)
collateral (DepCol _ (vb,vd) (_,_) ) (n1,n2) = (n1+vb, n2+vd)
collateral _                         (n1,n2) = (n1,n2)


nPriChoice :: Cx -> Int
nPriChoice (PriChoice cx1 cx2) = 1 + nPriChoice cx1 + nPriChoice cx2
nPriChoice _                   = 0

-- | this function takes as input a G describing contract preconditions
-- and outputs 2 vectors of extra secrets as describind in the following lines
-- each vector has length n * m 
-- where n the number of participants and m the number of priority choices
-- indexing the vector as  i * m / 0 <= i <= n - 1
-- gives as the first secret commited (the secret for the first level) by each user
lSecrets :: Gx -> V.Vector (Sname, Pname) -> V.Vector (Sname, Pname) -> (V.Vector (Sname, Pname), V.Vector (Sname, Pname))
lSecrets (SecGx gx1 gx2) v1 v2    = let v' = V.empty :: V.Vector (Sname, Pname)
                                        (v1', v2')   = lSecrets gx1 v' v'
                                        (v1'', v2'') = lSecrets gx2 v1 v2
                                    in  (v1' V.++ v1'', v2' V.++ v2'')      
lSecrets (SecretPlusB p xs) v1 v2 = let xs' = map (updateTuple2 p) xs in (V.fromList xs' V.++ v1, v2)
lSecrets (SecretPlusD p xs) v1 v2 = let xs' = map (updateTuple2 p) xs in (v1, V.fromList xs' V.++ v2)
lSecrets _ v1 v2                  = (v1,v2)                           

main :: IO ()
main = do
        let 
            v = V.empty :: V.Vector (Sname,Pname)                       -- init empty vector
            n = nPar g 0                        -- number of participants
            (u1,u2) = balance g (0,0)           -- balance of contract
            (col1,col2) = collateral g (0,0)    -- collaterals commited
            m = nPriChoice cSimpleTest          -- number of priority choices
            p = lPar g []                       -- list of participants' names
            (v1,v2) = lSecrets g v v            -- list (vector) of secrets' names
            t = check g n m u1 u2 p             -- check if contract preconditions are well defined
            c' = compileC cSimpleTest u1 u2 col1 col2 n m p 1 v1 v2     -- compile contract
        --print v1
        --print v2
        print m
        when t (print c')
        -- print c' 

-- | auxiliary stuff



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