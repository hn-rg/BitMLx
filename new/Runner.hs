{-|
Module      : Runner
Description : Entry point for running the compiler.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ParallelListComp #-}
module Runner where

import Syntax
import Compiler
import Auxiliary
import Pretty

import Data.Maybe
import Data.Map hiding (map,foldr)
import qualified Data.Map as Map
import qualified Data.Vector as V

import Prettyprinter.Render.Text 
import Prettyprinter.Internal
import Data.Text.Lazy.IO as TL

-- | Runs the compiler for a given scenario using predefined initial values. It then 
-- renders the resulting contract to text and writes them to specified file paths.
--
-- TODO: this should probably return the (maybe) rendered contracts as strings,
-- or even the actual contracts and leave the file writing as a concern for the app.
runCompiler :: Pl -> Gxl -> Cx -> String -> String -> IO ()
runCompiler participants preconditions contract outB outD = do
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
    print m
    when t (TL.writeFile outB renderB)
    when t (TL.writeFile outD renderD)


---------------------------------------
-- Initial variables for the compiler
---------------------------------------

-- | Extra time given to check if someone has cheated
tCheat = 10 :: Time

-- | Initial time
tInit  = 1 :: Time

-- | Initial level
level  = 1 :: Level

-- Initial balance
bal    = 0 :: Vd                                 

-- | For secrets
v      = V.empty :: V.Vector (Pname, Sname)

-- | For bitcoin deposits
v1     = V.empty :: V.Vector (Pname, Vb)

-- | For othercoin deposits
v2     = V.empty :: V.Vector (Pname, Vd)

-- | For bitcoin volatile deposits
vd1    = Map.empty :: Map.Map Xb Vb

-- | For othercoin volatile deposits
vd2    = Map.empty :: Map.Map Xd Vd