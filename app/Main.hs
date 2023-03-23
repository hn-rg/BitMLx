module Main where

import Prettyprinter.Render.Text ( renderLazy ) 
import Prettyprinter.Internal
    ( layoutPretty, defaultLayoutOptions )
import Data.Text.Lazy.IO as TL ( writeFile )

import Compiler ( compileC, compileD, compilePreconditions, initialSettings )
import Pretty ( prettyprintNL )

import Examples.PriorityChoice
    ( exampleName, participants, preconditions, contract )

main :: IO ()
main = do
    let (bG, dG) = compilePreconditions preconditions
        settings = initialSettings preconditions
    case compileC settings contract of
        Right (bD, dD) -> do
            let            
                bitcoinOutPath = "output/" ++ exampleName ++ "_bitcoin.rkt"
                dogecoinOutPath = "output/" ++ exampleName ++ "_dogecoin.rkt"

                docB = prettyprintNL participants bG bD 
                docD = prettyprintNL participants dG dD
                renderB = renderLazy (layoutPretty defaultLayoutOptions docB)
                renderD = renderLazy (layoutPretty defaultLayoutOptions docD)
            TL.writeFile bitcoinOutPath renderB
            TL.writeFile dogecoinOutPath renderD
        Left (errorType, errorDetails) -> print $ show errorType ++ ": " ++ errorDetails
