{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

  module Main where

import Syntax
import Examples
import Data.Char
import Data.List
import GHC.TypeLits

import Control.Monad.ST
import Data.Function
import Data.Bifunctor as DB


{- _____ Goal _____

        BitMLx ---> / preprocessing / ---> / compilation / ---> BitML||
   ________________
-}


-- well - formness of collaterals and extra secrets
{- ________
  1. EACH participant commits to m extra secrets 
        / m = # PriChoice
  2. EACH participant gives an extra deposit of value (n-1) * u 
        / n = # participants, u = initial balance of the contract
-}


check :: Gx -> Int -> Int -> Vb -> Vd -> [Pname] -> Bool
check g n m u1 u2 p =
        let x = checkSec g m [] 
        in 
                let y = checkCol g n u1 u2 [] 
                in fst x && sameElems (snd x) p && fst y && sameElems (snd y) p
        
        where 
        checkSec :: Gx -> Int -> [Pname] -> (Bool, [Pname])
        checkSec (SecGx gx1 gx2) m ps   =   let (b1,b2) = (checkSec gx1 m [], checkSec gx2 m ps)
                                            in DB.bimap (fst b1 &&) (snd b1 ++) b2
        checkSec (SecretPlus p xs) m ps =   (length xs == m, p:ps)
        checkSec _ _ ps                 =   (True,ps)

        checkCol :: Gx -> Int -> Vb -> Vd -> [Pname] -> (Bool, [Pname])
        checkCol (SecGx gx1 gx2) n u1 u2 ps             =  let (b1,b2) = (checkCol gx1 n u1 u2 [], checkCol gx2 n u1 u2 ps)
                                                           in DB.bimap (fst b1 &&) (snd b1 ++) b2
        checkCol (DepCol p (u1,u2) (x1,x2) ) n ub ud ps =  (u1 == (n-1)*ub && u2 == (n-1)*ud , p:ps) 
        checkCol _ _ _ _ ps                             = (True, ps)                                     


-- number of participants in the contract
nPar ::  Gx -> Int -> Int
nPar (SecGx gx1 gx2)       n = nPar gx1 n + nPar gx2 n
nPar (Depx _ (_,_) (_,_) ) n = n+1
nPar _                     n = n

-- list with the names of all participants in the contract
lPar ::  Gx -> [Pname] -> [Pname]
lPar (SecGx gx1 gx2)       ps = lPar gx1 [] ++ lPar gx2 ps
lPar (Depx p (_,_) (_,_) ) ps = p:ps
lPar _                     ps = ps

-- balance of the contract
-- mind that we have one for bitcoin, one for dogecoin
-- so the balance is a tuple of balances, for now
balance :: Gx -> (Vb,Vd) -> (Vb,Vd)
balance (SecGx gx1 gx2)         (n1,n2) = balance gx1 (n1,n2) ^+ balance gx2 (n1,n2)
balance (Depx _ (vb,vd) (_,_) ) (n1,n2) = (n1+vb, n2+vd)
balance _                       (n1,n2) = (n1,n2)


nPriChoice :: Cx -> Int
nPriChoice (PriChoice cx1 cx2) = 1 + nPriChoice cx1 + nPriChoice cx2
nPriChoice _                   = 0



main :: IO ()
main = do
        let n = nPar g 0
        let (u1,u2) = balance g (0,0)
        let m = nPriChoice c
        let p = lPar g []
        print p
        let t = check g n m u1 u2 p
        print t

-- auxiliary stuff

when :: MonadFail m => Bool -> m a -> m a
when True p  = p
when False _ = fail "when failed"

(^+) :: Num a => (a,a) -> (a,a) -> (a,a)
(x1,x2) ^+ (y1,y2) = (x1+y1, x2+y2)

-- checks if two lists have the same elements, 
-- even if they do not appear in the same order
sameElems :: (Eq a) => [a] -> [a] -> Bool
sameElems x y = null (x \\ y) && null (y \\ x)
