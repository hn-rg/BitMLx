module Main where

import Prettyprinter.Render.Text ( renderLazy ) 
import Prettyprinter.Internal
    ( layoutPretty, defaultLayoutOptions )
import Data.Text.Lazy.IO as TL ( writeFile )

import Pretty ( prettyprintNL )
import Syntax.BitML as BitML
import Syntax.BitMLx as BitMLx

import Examples.Escrow
    ( exampleName, participants, sourceAdvertisement)
import Compiler ( compileBitMLx )

main :: IO ()
main = case compileBitMLx sourceAdvertisement of
        (Right (BitML.ContractAdvertisement bitcoinPreconditions bitcoinContract, BitML.ContractAdvertisement dogecoinPreconditions dogecoinContract)) -> do
            let            
                bitcoinOutPath = "output/" ++ exampleName ++ "_bitcoin.rkt"
                dogecoinOutPath = "output/" ++ exampleName ++ "_dogecoin.rkt"

                bitcoinDoc = prettyprintNL participants bitcoinPreconditions bitcoinContract 
                dogecoinDoc = prettyprintNL participants dogecoinPreconditions dogecoinContract
                renderBitcoin = renderLazy (layoutPretty defaultLayoutOptions bitcoinDoc)
                renderDogecoin = renderLazy (layoutPretty defaultLayoutOptions dogecoinDoc)
            TL.writeFile bitcoinOutPath renderBitcoin
            TL.writeFile dogecoinOutPath renderDogecoin
        (Left compilationError) -> print $ show compilationError
