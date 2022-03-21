{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ParallelListComp #-}
module Main where

import Syntax
import Examples
import Trifunctor
import Auxiliary
import Pretty

import Data.Char
import Data.Maybe
import Data.Map hiding (map,foldr)
import qualified Data.Map as Map
--import Data.List hiding (sort)
import GHC.TypeLits

import Control.Monad.ST
--import Data.Function
import Data.Bifunctor as DB

import qualified Data.Vector as V

import qualified Data.Vector.Algorithms.Intro as I
import Prettyprinter.Util

{- _____ Goal _____

        BitMLx ---> / preprocessing / ---> / compilation / ---> BitML||
   ________________
-}

-- | initial values
tCheat = 100                                -- extra time given to check if someone has cheated
tInit  = 1                                  -- initial time
level  = 1                                  -- initial level
bal    = 0                                  -- initial balance
v      = V.empty :: V.Vector (Pname, Sname) -- for secrets
v1     = V.empty :: V.Vector (Pname, Vb)    -- for bitcoin deposits
v2     = V.empty :: V.Vector (Pname, Vd)    -- for othercoin deposits
vd1    = Map.empty :: Map.Map Xb Vb       -- for bitcoin volatile deposits
vd2    = Map.empty :: Map.Map Xd Vd       -- for othercoin volatile deposits


-- | INPUTS:
--   contract: Cx 
-- , balance for CURRENT blockchain: u
-- , collaterals for CURRENT blockchain: uCol,
-- , # participants: n
-- , # priority choices: m
-- , list of participants: ps
-- , current level of execution i
-- , secrets vector for CURRENT blockchain: s1
-- , secrets vector for OTHER blockchain: s2
-- , deposits vector for CURRENT blockchain: dep
-- , current time: t
-- , flag: True for Bitcoin, False for Dogecoin

-- | OUTPUT
-- , compiled contract: C

compileC :: Cx -> V -> V -> Int -> Int
    -> [Pname] -> Level
    -> V.Vector (Pname, Sname)
    -> V.Vector (Pname, Sname)
    -> V.Vector (Pname, V)
    -> Map.Map X V
    -> Time
    -> Bool
    -> C
compileC ( Withdrawx p : d) u uCol n m ps i s1 s2 dep vdep t flag = 
    case d of []       ->  c
          
              (x : _)  ->  c2
    where   
        
        -- | if we have no other priority choice following (d == []), we take that choice without revealing any extra secret 
        -- | because if there is at least one honest user in the setting she could do the step without any extra auth
        c  = [ Split ( u : replicate (uCol `div` n) n ) ( [Withdraw p] : [ [Withdraw i] | i <- ps] ) ]   -- bitcoin  
       
        -- | this is the first big choice of the compiled contract, where everything goes as expected, 2 remainning 
        -- | here we just have a choice bewteen actions (reveals). Any user can reveal her extra secret of that level to stipulate c1 (c2 in dogecoin)
        -- | we concatenate all the possible reveal contracts
        -- | the number of possible reveals equals to the number of par/ants, the result of "concatChoices" is a list of contracts w\ lentgh n
        d1  = concatChoices c s1 n m i 1                  -- bitcoin

        (c1, c2) = create2ExtraChoices d1 d u uCol n m ps i s1 s2 dep vdep t flag


compileC (Splitx listU listC : d) u uCol n m ps i s1 s2 dep vdep t flag = 
    case d of []       ->  d'

              (x : _)  ->  c2
    where
        listCol =  map (^* (n * (n - 1), n * (n - 1)) )  listU -- [ (n * (n-1) * ui, n * (n-1) * uj ) | (ui, uj) <- listU]  
        listU'  =  [ (ui + n * (n - 1) * ui, uj + n * (n - 1) * uj ) | (ui, uj) <- listU]
        (u1, u2) = unzip listU'


        -- | someone reveals so we execute the first priority choice (split)
        -- | if we have no other priority choice following (d == []), we take that choice without revealing any extra secret 
        -- | because if there is at least one honest user in the setting she could do the step without any extra auth
        c  = if flag 
                then [compileC di u uCol n m ps i s1 s2 dep vdep t True  | di <- listC | (u,_) <- listU | (uCol, _) <- listCol]  -- compile its di with the same i and not (i+1) since we dont require here revealing extra secret
                else [compileC di u uCol n m ps i s1 s2 dep vdep t False | di <- listC | (_,u) <- listU | (_, uCol) <- listCol]  -- compile its di with the same i and not (i+1) since we dont require here revealing extra secret

        d' = if flag
                then [Split u1 c]
                else [Split u2 c]

        -- | this is the first big choice of the compiled contract, where everything goes as expected, 2 remainning 
        -- | here we just have a choice bewteen actions (reveals). Any user can reveal her extra secret of that level to stipulate c1 (c2 in dogecoin)
        -- | we concatenate all the possible reveal contracts
        -- | the number of possible reveals equals to the number of par/ants, the result of "concatChoices" is a list of contracts w\ lentgh n
        d1  = concatChoices d' s1 n m i 1                  -- bitcoin
        
        k = foldr1 (\x y ->if x >= y then x else y) (map nPriChoices listC)

        (c1, c2) = create2ExtraChoices d1 d u uCol n m ps (i+k) s1 s2 dep vdep t flag

        
compileC (Authx p d : ds) u uCol n m ps i s1 s2 dep vdep t flag =
    case ds of []        -> c1
               (_ : xs)  -> c2

    where 
        -- | here contract list has just one element, recursion ends
        -- | but we still need to reveal an extra secret and the cheating mechanism
        -- | to protect honest users from adversaries giving their authorization only in one blockchain    
        c = compileC [d] u uCol n m ps (i+1) s1 s2 dep vdep t flag       
        
        k = nPriChoices [d]
        -- | first compile the case where someone reveals a secret so steps to the first priority choice
        d1  =  concati c s1 n m i 1 (Auth p (head c) )               
   
        (c1, c2) = create2ExtraChoices d1 ds u uCol n m ps (i+k) s1 s2 dep vdep t flag


compileC (Putx x cs : ds) u uCol n m ps i s1 s2 dep vdep t flag =
    case ds of 
        []         ->  c1
        (_ : xs)   ->  c2        -- compiled into a PutRev contract (atomic put and reveal)

    where 
        -- | here contract list has just one element, recursion ends    
        -- | but we still need to reveal an extra secret and the cheating mechanism
        -- | to protect honest users from adversaries giving their authorization only in one blockchain    
        (x1, x2) = unzip x
        voldeps = if flag
                    then toAddVolDeps x1 vdep
                    else toAddVolDeps x2 vdep
        c = compileC cs (u+voldeps) uCol n m ps (i+1) s1 s2 dep vdep t flag       -- compile with the same i for level
        
        -- | *** ADD VOLATILE DEPOSITS IN BALANCE u WHEN COMPILING c ABOVE -> done

        -- | first compile the case where someone reveals a secret so steps to the first priority choice
        d1  = if flag 
                then concati c s1 n m i 1 (Put x1 c )
                else concati c s1 n m i 1 (Put x2 c )
        
        k = nPriChoices cs
        (c1, c2) = create2ExtraChoices d1 ds u uCol n m ps (i+k) s1 s2 dep vdep t flag


compileC (Revealx a cs : ds) u uCol n m ps i s1 s2 dep vdep t flag =
    case ds of 
        []         ->  c1
        (_ : xs)   ->  c2         -- compiled into a Rev contract (atomic  reveal)

    where 
        -- | here contract list has just one element, recursion ends    
        -- | but we still need to reveal an extra secret and the cheating mechanism
        -- | to protect honest users from adversaries giving their authorization only in one blockchain              
        c = compileC cs u uCol n m ps (i+1) s1 s2 dep vdep t flag       -- compile with the same i for level
        
        -- | first compile the case where someone reveals a secret so steps to the first priority choice
        d1  =  concati c s1 n m i 1 (Reveal a c )             -- bitcoin 

        k = nPriChoices cs
        (c1, c2) = create2ExtraChoices d1 ds u uCol n m ps (i+k) s1 s2 dep vdep t flag


compileC (Revealifx a e cs : ds) u uCol n m ps i s1 s2 dep vdep t flag =
    case ds of 
        []         ->  c1
        (_ : xs)   ->  c2         -- compiled into a Rev contract (atomic reveal)

    where  
        -- | here contract list has just one element, recursion ends    
        -- | but we still need to reveal an extra secret and the cheating mechanism
        -- | to protect honest users from adversaries giving their authorization only in one blockchain                       
        c = compileC cs u uCol n m ps (i+1) s1 s2 dep vdep t flag       -- compile with the same i for level
        
        -- | first compile the case where someone reveals a secret so steps to the first priority choice
        d1  =  concati c s1 n m i 1 (Revealif a e c)             -- bitcoin 

        k = nPriChoices cs
        (c1, c2) = create2ExtraChoices d1 ds u uCol n m ps (i+k) s1 s2 dep vdep t flag

compileC (PutRevx x a cs : ds) u uCol n m ps i s1 s2 dep vdep t flag =
    case ds of 
        []         ->  c1
        (_ : xs)   ->  c2         -- compiled into a PutRev contract (atomic put and reveal)

    where  
        
        (x1,x2) = unzip x 
        voldeps = if flag
                    then toAddVolDeps x1 vdep
                    else toAddVolDeps x2 vdep
        c =  compileC cs (u+voldeps) uCol n m ps (i+1) s1 s2 dep vdep t flag       -- compile with the same i for level
        
        -- | first compile the case where someone reveals a secret so steps to the first priority choice
        d1  = if flag 
                then concati c s1 n m i 1 (PutRev x1 a c)             -- bitcoin 
                else concati c s1 n m i 1 (PutRev x2 a c)
        
        k = nPriChoices cs
        (c1, c2) = create2ExtraChoices d1 ds u uCol n m ps (i+k) s1 s2 dep vdep t flag


compileC (PutRevifx x a e cs : ds) u uCol n m ps i s1 s2 dep vdep t flag =
    case ds of 
        []         ->  c1
        (_ : xs)   ->  c2         -- compiled into a PutRev contract (atomic put and reveal)

    where  
        (x1,x2) = unzip x 
        voldeps = if flag
                    then toAddVolDeps x1 vdep
                    else toAddVolDeps x2 vdep
        c =  compileC cs (u+voldeps) uCol n m ps (i+1) s1 s2 dep vdep t flag     -- compile with the same i for level
        
        -- | first compile the case where someone reveals a secret so steps to the first priority choice
        d1  = if flag 
                then concati c s1 n m i 1 (PutRevif x1 a e c)             -- bitcoin 
                else concati c s1 n m i 1 (PutRevif x2 a e c) 

        k = nPriChoices cs

        (c1, c2) = create2ExtraChoices d1 ds u uCol n m ps (i+k) s1 s2 dep vdep t flag


-- | INPUTS:
--  contract: d1 (first compiled choice)
-- , contract: Cx 
-- , balance for CURRENT blockchain: u
-- , collaterals for CURRENT blockchain: uCol,
-- , # participants: n
-- , # priority choices: m
-- , list of participants: ps
-- , current level of execution i
-- , secrets vector for CURRENT blockchain: s1
-- , secrets vector for OTHER blockchain: s2
-- , deposits vector for CURRENT blockchain: dep
-- , current time: t
-- , flag: True for Bitcoin, False for Dogecoin

-- | OUTPUT
-- , compiled contracts: c1,c2

create2ExtraChoices :: C -> Cx -> V -> V -> Int -> Int
    -> [Pname] -> Level
    -> V.Vector (Pname, Sname)
    -> V.Vector (Pname, Sname)
    -> V.Vector (Pname, V)
    -> Map.Map X V
    -> Time
    -> Bool
    ->(C,C)

create2ExtraChoices d1 ds u uCol n m ps i s1 s2 dep vdep t flag = 
    let    
        -- | this is the 2nd big choice of the compiled contract, where we check if someone has cheated
        -- | we concatenate all the possible cheat cases, where someone has revealed in only one of the two blockchains
        -- | the number of possible cheat cases equals to the number of par/ants, the result of "cheatCase" is a list of contracts w\ lentgh n  
        d2  = cheatCase ps n u uCol dep s2 i m 1 t
        
        -- | here contract list has more than one element, compile recursively
        -- | this is the 3rd and last big choice of the compiled contract, where noone revealed so we move to the next priority choice to be executed
        d3   = compileC ds u uCol n m ps (i+1) s1 s2 dep vdep (i*t + tCheat) flag
        d3'  = map (After (i * t + tCheat)) d3

        -- | compiled contracts c1 if contract list= [] else c2
        c1 = d1 ++ d2
        c2 = c1 ++ d3'

    in (c1,c2)    

concati :: C -> V.Vector (Pname, Sname) -> Int -> Int -> Int -> Int -> D -> C
concati d s n m i j x@(Withdraw _)
    | j <= n    = Reveal [snd $ s V.! ((j - 1) * m  + i - 1  ) ] d : concati d s n m i (j + 1) x
    | otherwise = []
concati d s n m i j x@(Split _ _)
    | j <= n    = Reveal [snd $ s V.! ((j - 1) * m  + i - 1  ) ] d : concati d s n m i (j + 1) x
    | otherwise = []
concati d s n m i j x@(Auth p _)
    | j <= n    = Auth p ( Reveal [snd $ s V.! ((j - 1) * m  + i - 1  ) ] d ) : concati d s n m i (j + 1) x
    | otherwise = []
concati d s n m i j x@(Put xs _)
    | j <= n    = PutRev xs [snd $ s V.! ((j - 1) * m  + i - 1  ) ] d  : concati d s n m i (j + 1) x
    | otherwise = []             
concati d s n m i j x@(PutRev xs as _)
    | j <= n    = PutRev xs ( (snd $ s V.! ((j - 1) * m  + i - 1  ) ) : as ) d  : concati d s n m i (j + 1) x
    | otherwise = []
concati d s n m i j x@(PutRevif xs as e _)
    | j <= n    = PutRevif xs ( (snd $ s V.! ((j - 1) * m  + i - 1  ) ) : as ) e d  : concati d s n m i (j + 1) x
    | otherwise = []
concati d s n m i j x@(Reveal as _)
    | j <= n    = Reveal ( (snd $ s V.! ((j - 1) * m  + i - 1  ) ) : as ) d  : concati d s n m i (j + 1) x
    | otherwise = [] 
concati d s n m i j x@(Revealif as e _)
    | j <= n    = Revealif ( (snd $ s V.! ((j - 1) * m  + i - 1  ) ) : as ) e d  : concati d s n m i (j + 1) x
    | otherwise = []     

atomicPutRev :: C -> V.Vector (Pname, Sname) -> Int -> Int -> Int -> Int -> [X] -> C
atomicPutRev d s n m i j xs
    | j <= n    = PutRev xs [snd $ s V.! ((j - 1) * m  + i - 1  ) ] d : atomicPutRev d s n m i (j + 1) xs
    | otherwise = [] 
              

-- | i is level counter, j is participant counter (IMPORTANT: INIT VALUE -> 1)
-- n is the number of participants, m is the number of prichoices

-- | this function takes as input a contract and a vector of secrets and creates a new contract
-- which consist of as many guarded contracts as the number of participants
-- each participant can stipulate the initial contract, providing her secret
concatChoices :: C -> V.Vector (Pname, Sname) -> Int -> Int -> Int -> Int -> C
concatChoices d s n m i j
    | j <= n    = Reveal [snd $ s V.! ((j - 1) * m  + i - 1  ) ] d : concatChoices d s n m i (j + 1)
    | otherwise = []
-- | vector of secrets can be seen as a 2D array
-- | rows index is for participants, columns index is for the different levels
-- | rows index goes up to n, columns index goes up to m
-- | each time we want to index to the next participants secret we have to add m
-- for example if we want the secrets of the 3rd participant we add m 3 times => 3*m
-- if we want the 2nd secret of the 3rd participant we just add 2 => 3*m + 2 


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

toAddVolDeps :: [X] -> Map.Map X V -> V
toAddVolDeps xs v = foldr (\x y -> v Map.! x + y) 0 xs

-- | well - formness of collaterals and extra secrets
{- | ________
  1. EACH participant commits to m extra secrets 
        / m = # PriChoice
  2. EACH participant gives an extra deposit of value (n-1) * u 
        / n = # participants, u = initial balance of the contract
-}

check :: Gxl -> Int -> Int -> Vb -> Vd -> [Pname] -> Bool
check g n m u1 u2 p 
    | m == 0    = let (b,x) = aux True g n m u1 u2 [] [] []                 -- no need for extra secrets if we dont have priority choices
                    in b && sameElems p (thd x)
    | otherwise = let (b,x) = aux True g n m u1 u2 [] [] []
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


-- | INPUTS
--   contract preconditions Gxl
-- , bitcoin balance n1
-- , dogecoin balance n2
-- , bitcoin collaterals : col1
-- , dogecoin collaterals: col2
-- , tuple vector for participants and their deposit in bitcoin : p1
-- , tuple vector for participants and their deposit in dogecoin : p2
-- , tuple vector for volatile deposit names and their value in bitcoin : vol1
-- , tuple vector for volatile deposit names and their value in dogecoin : vol2

balCol :: Gxl -> Vb -> Vd -> Vb -> Vd 
        -> V.Vector (Pname, Vb) -> V.Vector (Pname, Vd)
        -> Map.Map Xb Vb -> Map.Map Xd Vd
        -> (Vb, Vd, Vb, Vd, V.Vector (Pname, Vb), V.Vector (Pname, Vb), Map.Map Xb Vb, Map.Map Xd Vd)
balCol []                               n1 n2 col1 col2 p1 p2 vol1 vol2 = (n1, n2, col1, col2, p1, p2, vol1, vol2)
balCol (Depx p (vb,vd) (_,_) : xs)      n1 n2 col1 col2 p1 p2 vol1 vol2 = balCol xs (n1 + vb) (n2 + vd) col1 col2 (V.cons (p,vb) p1) (V.cons (p,vd) p2) vol1 vol2
balCol (DepCol p (vb,vd) (_,_) : xs)    n1 n2 col1 col2 p1 p2 vol1 vol2 = balCol xs n1 n2 (col1 + vb) (col2 + vd) p1 p2 vol1 vol2
balCol (VolDepx _ (vb,vd) (xb,xd) : xs) n1 n2 col1 col2 p1 p2 vol1 vol2 = balCol xs n1 n2 col1 col2 p1 p2 (Map.insert xb vb vol1) (Map.insert xd vd vol2)
balCol ( _ : xs)                        n1 n2 col1 col2 p1 p2 vol1 vol2 = balCol xs n1 n2 col1 col2 p1 p2 vol1 vol2


-- | this function takes as input a G describing contract preconditions
-- and outputs 2 vectors of extra secrets as describind in the following lines
-- each vector has length n * m, where n the number of participants and m the number of priority choices
-- indexing the vector as  i * m / 0 <= i <= n - 1, gives as the first secret commited (the secret for the first level) by each user
lSecrets :: Gxl -> V.Vector (Pname, Sname) -> V.Vector (Pname, Sname) -> (V.Vector (Pname, Sname), V.Vector (Pname, Sname))
lSecrets []                       v1 v2 = (v1, v2)
lSecrets ( SecretPlusB p s : xs ) v1 v2 = let s' = map (updateTuple2 p) s in lSecrets xs (V.fromList s' V.++ v1) v2
lSecrets ( SecretPlusD p s : xs ) v1 v2 = let s' = map (updateTuple2 p) s in lSecrets xs v1 (V.fromList s' V.++ v2)
lSecrets ( _ : xs )               v1 v2 = lSecrets xs v1 v2


-- | count the number of priority choices recursively for every contract
-- | even for them which are guarded by a reveal
-- | and for them created by a split
nPriChoices ::  Cx  -> Int
nPriChoices []                       = 0
nPriChoices [Withdrawx _ ]           = 0
nPriChoices [Splitx _ cs]            = sum (map nPriChoices  cs)
nPriChoices (Putx _ c : xs)          = 1 + nPriChoices c + nPriChoices xs
nPriChoices (Revealx _ c : xs)       = 1 + nPriChoices c + nPriChoices xs
nPriChoices (Revealifx _ _ c : xs)   = 1 + nPriChoices c + nPriChoices xs
nPriChoices (PutRevx _ _ c : xs)     = 1 + nPriChoices c + nPriChoices xs
nPriChoices (PutRevifx _ _ _ c : xs) = 1 + nPriChoices c + nPriChoices xs
nPriChoices (Splitx _ cs : xs)       = let m = sum (map nPriChoices  cs)
                                       in 1 + m + nPriChoices  xs
nPriChoices (Withdrawx _ : xs)       = 1 + nPriChoices  xs
nPriChoices (Authx _ d : xs)         = 1 + nPriChoices [d] + nPriChoices xs




main :: IO ()
main = do
    let
        n = length p1                        -- number of participants
        (u1, u2, col1, col2, dep1, dep2, vol1, vol2) = balCol g1 bal bal bal bal v1 v2 vd1 vd2          -- balance of contract + collaterals
        m = nPriChoices c2          -- number of priority choices
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
        cB = compileC c2 u1  col1  n m p' level s1' s2' dep1' vol1 tInit True  -- compile contract IN BITCOIN
        cD = compileC c2 u2  col2  n m p' level s2' s1' dep2' vol2 tInit False -- compile contract IN DOGECOIN
        doc = prettyprin c3
    putDocW 80 doc
    --print m
    --when t (print t)
    --when t (print cB )
    --when t (print cD )
        

