{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Syntax
import Examples
import Trifunctor
import Auxiliary

import Data.Char
--import Data.List hiding (sort)
import GHC.TypeLits

import Control.Monad.ST
--import Data.Function
import Data.Bifunctor as DB

import qualified Data.Vector as V

import qualified Data.Vector.Algorithms.Intro as I
{- _____ Goal _____

        BitMLx ---> / preprocessing / ---> / compilation / ---> BitML||
   ________________
-}

-- | initial values
tCheat = 100                            -- extra time given to check if someone has cheated
tInit = 1                               -- initial time
level = 1                               -- initial level
bal = 0                                 -- initial balance
v = V.empty :: V.Vector (Pname, Sname)  -- for secrets
v1 = V.empty :: V.Vector (Pname, Vb)    -- for bitcoin deposits
v2 = V.empty :: V.Vector (Pname, Vb)    -- for othercoin deposits

-- | INPUTS:
-- | contract C, bitcoin's balance ub, dogecoin's balance ud, bitcoin's collaterals ubCol, dogecoin's collaterals ubCol, 
-- | # participants n, # priority choices m, list of participants ps, current level of execution i,
-- | secrets vector for bitcoin s1, secrets vector for dogecoin s2
-- | deposits vector for bitcoin s1, deposits vector for dogecoin s2
-- | current time t
-- 
compileC :: Cx -> Vb -> Vd -> Vb -> Vd -> Int -> Int
        -> [Pname] -> Level
        -> V.Vector (Pname, Sname)
        -> V.Vector (Pname, Sname)
        -> V.Vector (Pname, Vb)
        -> V.Vector (Pname, Vd)
        -> Time
        -> (C, C)
compileC [Withdrawx p] ub ud ubCol udCol n m ps i s1 s2 dep1 dep2 t = 
        let
            d'  = Split ( ub : [ubCol `div` n | i <- [1..n] ] ) ( [Withdraw p] : [ [Withdraw i] | i <- ps] )   -- bitcoin  
            dx' = Split ( ud : [udCol `div` n | i <- [1..n] ] ) ( [Withdraw p] : [ [Withdraw i] | i <- ps]  )  -- dogecoin
        in ([d'],[dx'])    
compileC ( Withdrawx p : d) ub ud ubCol udCol n m ps i s1 s2 dep1 dep2 t =
        let
            d'  = Split ( ub : [ubCol `div` n | i <- [1..n] ] ) ( [Withdraw p] : [ [Withdraw i] | i <- ps] )   -- bitcoin  
            dx' = Split ( ud : [udCol `div` n | i <- [1..n] ] ) ( [Withdraw p] : [ [Withdraw i] | i <- ps]  )  -- dogecoin

            -- | this is the first big choice of the compiled contract, where everything goes as expected, 2 remainning 
            d1  = concatChoices [d'] s1 n m i 1                  -- bitcoin
            d1x = concatChoices [dx'] s2 n m i 1                -- dogecoin

            d2  = cheatCase ps n ub ubCol dep1 s2 i m 1 t
            d2x = cheatCase ps n ud udCol dep2 s1 i m 1 t

            (d3, d3x) = compileC d ub ud ubCol udCol n m ps (i+1) s1 s2 dep1 dep2 (i*t + tCheat)
            d3'  = map (After (i * t + tCheat)) d3
            d3x' = map (After (i * t + tCheat)) d3x

            dnew  = d1  ++ d2 ++ d3'
            dnewx = d1x ++ d2x ++ d3x'

        in (dnew, dnewx)


-- | WE NEED TO CARE about the COLLATERALS bc WE NEED TO KEEP THE INVARIANT Σ ui = balance
-- | where balance of C = U + COLLATERALS!
-- | IN THAT SENSE THE FOLLOWING IS WRONG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- | we should compile split in a different way inserting somehow the collaterals in an extra contract
-- imagine that we have split [1,1] -> [c1,c2]
-- we do split [1,1, collaterals] -> [compiled-c1, compiled-c2, withdraw-collaterals]
compileC [Splitx listU listC ] ub ud ubCol udCol n m ps i s1 s2 dep1 dep2 t = 
        let 
            cs       =  [compileC di ub ud ubCol udCol n m ps (i+1) s1 s2 dep1 dep2 t | di <- listC ]  
            (c1,c2)  = unzip cs
            (u1, u2) = unzip listU
        in ([Split u1 c1 ], [Split u2 c2] )    
compileC (Splitx listU listC : d) ub ud ubCol udCol n m ps i s1 s2 dep1 dep2 t =   
        let
            -- | someone reveals so we execute the first priority choice (split)
            cs       =  [compileC di ub ud ubCol udCol n m ps (i+1) s1 s2 dep1 dep2 t | di <- listC ]  
            (c1,c2)  = unzip cs
            (u1, u2) = unzip listU
            d'       = Split u1 c1
            dx'      = Split u2 c2

            -- | this is the first big choice of the compiled contract, where everything goes as expected, 2 remainning 
            d1  = concatChoices [d'] s1 n m i 1                  -- bitcoin
            d1x = concatChoices [dx'] s2 n m i 1                -- dogecoin
            
            d2  = cheatCase ps n ub ubCol dep1 s2 i m 1 t
            d2x = cheatCase ps n ud udCol dep2 s1 i m 1 t

            (d3, d3x) = compileC d ub ud ubCol udCol n m ps (i+1) s1 s2 dep1 dep2 (i*t + tCheat)
            d3'  = map (After (i * t + tCheat)) d3
            d3x' = map (After (i * t + tCheat)) d3x

            dnew  = d1 ++ d2 ++ d3'
            dnewx = d1x ++ d2x ++ d3x'

        in (dnew, dnewx)
-- compileC ()        


-- | i is level counter, j is participant counter (IMPORTANT: INIT VALUE -> 1)
-- n is the number of participants, m is the number of prichoices

-- | this function takes as input a contract and a vector of secrets and creates a new contract
-- which consist of as many guarded contracts as the number of participants
-- each participant can stipulate the initial contract, providing her secret
concatChoices :: C -> V.Vector (Pname, Sname) -> Int -> Int -> Int -> Int -> C
concatChoices d s n m i j
        | j <= n    = Reveal [snd $ s V.! (i - 1 + m * (j - 1) ) ] d : concatChoices d s n m i (j + 1)
        | otherwise = []


-- | i is level counter, j is participant counter
-- n is the number of participants, m is the number of prichoices
cheatCase ::  [Pname] -> Int -> Int -> Int
        -> V.Vector (Pname, Int) -> V.Vector (Pname, Sname)
        -> Level -> Int -> Int -> Time -> C
cheatCase ps n u ucol dep s i m j t
        | j <= n   = k : cheatCase ps n u ucol dep s i m (j + 1) t
        |otherwise = []

   where k = After (i * t) (Reveal [snd $ s V.! p]                              -- check times !!!!!
                                [ Split  [ i + ucol `div` (n-1) | (_,i) <- ps'   ]        -- add deposit of Pi -> done
                                        [ [Withdraw i] | (i,_) <- ps' ] ] )   -- see *** below
         p = i - 1 + m * (j - 1)
         ps' = vecCompr (j-1) dep
-- | *** participants list is extracted from deposits list, 
-- | the order of appearance is the same in deposits list and secrets list, since we have sorted them.

-- | P  is the participant who is checked for cheating


vecCompr :: Int -> V.Vector (Pname, a) -> [(Pname, a)]
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
balCol :: Gxl -> Vb -> Vd -> Vb -> Vd -> V.Vector (Pname, Vb) -> V.Vector (Pname, Vd)
        -> (Vb, Vd, Vb, Vd, V.Vector (Pname, Vb), V.Vector (Pname, Vb))
balCol []                            n1 n2 col1 col2 p1 p2 = (n1, n2, col1, col2, p1, p2)
balCol (Depx p (vb,vd) (_,_) : xs)   n1 n2 col1 col2 p1 p2 = balCol xs (n1 + vb) (n2 + vd) col1 col2 (V.cons (p,vb) p1) (V.cons (p,vd) p2)
balCol (DepCol p (vb,vd) (_,_) : xs) n1 n2 col1 col2 p1 p2 = balCol xs n1 n2 (col1 + vb) (col2 + vd) p1 p2
balCol ( _ : xs)                     n1 n2 col1 col2 p1 p2 = balCol xs n1 n2 col1 col2 p1 p2


-- | this function takes as input a G describing contract preconditions
-- and outputs 2 vectors of extra secrets as describind in the following lines
-- each vector has length n * m, where n the number of participants and m the number of priority choices
-- indexing the vector as  i * m / 0 <= i <= n - 1, gives as the first secret commited (the secret for the first level) by each user
lSecrets :: Gxl -> V.Vector (Pname, Sname) -> V.Vector (Pname, Sname) -> (V.Vector (Pname, Sname), V.Vector (Pname, Sname))
lSecrets []                       v1 v2 = (v1, v2)
lSecrets ( SecretPlusB p s : xs ) v1 v2 = let s' = map (updateTuple2 p) s in lSecrets xs (V.fromList s' V.++ v1) v2
lSecrets ( SecretPlusD p s : xs ) v1 v2 = let s' = map (updateTuple2 p) s in lSecrets xs v1 (V.fromList s' V.++ v2)
lSecrets ( _ : xs )               v1 v2 = lSecrets xs v1 v2

nPriChoices ::  Cx  -> Int
nPriChoices []                       = 0
nPriChoices [Withdrawx _]            = 0
nPriChoices [Authx _ _ ]             = 0
nPriChoices [Putx _ c]               = nPriChoices c 
nPriChoices [Revealx _ c]            = nPriChoices c
nPriChoices [Revealifx _ _ c]        = nPriChoices c 
nPriChoices [PutRevx _ _ c]          = nPriChoices c 
nPriChoices [PutRevifx _ _ _ c]      = nPriChoices c 
nPriChoices [Splitx _ cs]            = sum (map nPriChoices  cs)
nPriChoices (Putx _ c : xs)          = 1 + nPriChoices c + nPriChoices xs
nPriChoices (Revealx _ c : xs)       = 1 + nPriChoices c + nPriChoices xs
nPriChoices (Revealifx _ _ c : xs)   = 1 + nPriChoices c + nPriChoices xs
nPriChoices (PutRevx _ _ c : xs)     = 1 + nPriChoices c + nPriChoices xs
nPriChoices (PutRevifx _ _ _ c : xs) = 1 + nPriChoices c + nPriChoices xs
nPriChoices (Splitx _ cs : xs)       = let m = sum (map nPriChoices  cs)
                                       in 1 + m + nPriChoices  xs
nPriChoices (Withdrawx _ : xs)       = 1 + nPriChoices  xs
nPriChoices (Authx _ _ : xs)         = 1 + nPriChoices xs







-- dep :: Gxl -> Vb -> Vd -> (V.Vector (Pname,Vb), V.Vector)
main :: IO ()
main = do
        let
            n = length p1                        -- number of participants
            (u1, u2, col1, col2, dep1, dep2) = balCol g1 bal bal bal bal v1 v2           -- balance of contract + collaterals
            m = nPriChoices cSimpleTest           -- number of priority choices
            p = map (\ (Par x y) -> x) p1        -- list of participants' names
            (s1,s2) = lSecrets g1 v v            -- list (vector) of secrets' names

            -- | sort participants list, secrets vectors, deposits vectors according to alphabetical order of participants names
            p' = sortList p
            s1' = sortVec s1
            s2' = sortVec s2
            dep1' = sortVec dep1
            dep2' = sortVec dep2

            -- | check well formness and then IF well formed -> compile
            t  = check g1 n m u1 u2 p             -- check if contract preconditions are well defined
            c' = compileC cSimpleTest u1 u2 col1 col2 n m p' level s1' s2' dep1' dep2' tInit  -- compile contract
        when t (print t)
        when t (print $ fst c')
        --when t (print $ snd c')
        

