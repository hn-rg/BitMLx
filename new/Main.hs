{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ParallelListComp #-}
module Main where

import Syntax
import Compiler
import Examples
import Auxiliary
import Pretty

import Data.Maybe
import Data.Map hiding (map,foldr)
import qualified Data.Map as Map
import qualified Data.Vector as V

import Prettyprinter.Render.Text 
import Prettyprinter.Internal
import Data.Text.Lazy.IO as TL

-- |     Goal :
--  BitMLx ---> / preprocessing / ---> / compilation / ---> BitML||

main :: IO ()
main = do
    let
        n = length participants                        -- number of participants
        (u1, u2, col1, col2, dep1, dep2, vol1, vol2) = balCol preconditions bal bal bal bal v1 v2 vd1 vd2          -- balance of contract + collaterals
        m = nPriChoices contract          -- number of priority choices
        p = map (\ (Par x y) -> x) participants        -- list of participants' names
        (s1,s2) = lSecrets preconditions v v            -- list (vector) of secrets' names

        -- | sort participants list, secrets vectors, deposits vectors according to alphabetical order of participants names
        p' = sortList p
        s1' = sortVec s1
        s2' = sortVec s2
        dep1' = sortVec dep1
        dep2' = sortVec dep2

        -- | check well formness and then IF well formed -> compile
        t  = check preconditions n m u1 u2 p             -- check if contract preconditions are well defined
        cB = compileC contract u1  col1  n m p' level s1' s2' dep1' vol1 tInit True  -- compile contract IN BITCOIN
        cD = compileC contract u2  col2  n m p' level s2' s1' dep2' vol2 tInit False -- compile contract IN DOGECOIN
        gB = compileG preconditions True
        gD = compileG preconditions False

        -- | use prettyprinter for results and write them to file
        docB = prettyprintNL participants gB cB 
        docD = prettyprintNL participants gB cD
        renderB = renderLazy (layoutPretty defaultLayoutOptions docB)
        renderD = renderLazy (layoutPretty defaultLayoutOptions docD)

    when t (TL.writeFile outB renderB)
    when t (TL.writeFile outD renderD)

    where 

        -- | INPUTS & OUTPUTS : IF YOU WANT TO TEST EDIT HERE THANK YOU :)
        participants = p1
        preconditions = g1
        contract = c1
        outB = "outB.rkt"
        outD = "outD.rkt"


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



