module Main where

import Prettyprinter.Render.Text ( renderLazy ) 
import Prettyprinter.Internal
    ( layoutPretty, defaultLayoutOptions )
import Data.Text.Lazy.IO as TL ( writeFile )

import Compiler ( compileD, compileG )
import Pretty ( prettyprintNL )

import Examples.Withdraw
    ( exampleName, participants, preconditions, contract )

main :: IO ()
main = do
    let (bG, dG) = compileG preconditions
    case compileD preconditions contract of
        Just (bD, dD) -> do
            let            
                bitcoinOutPath = "output/" ++ exampleName ++ "_bitcoin.rkt"
                dogecoinOutPath = "output/" ++ exampleName ++ "_dogecoin.rkt"

                docB = prettyprintNL participants bG bD 
                docD = prettyprintNL participants dG dD
                renderB = renderLazy (layoutPretty defaultLayoutOptions docB)
                renderD = renderLazy (layoutPretty defaultLayoutOptions docD)
            TL.writeFile bitcoinOutPath renderB
            TL.writeFile dogecoinOutPath renderD
        Nothing -> print "Compilation error"
