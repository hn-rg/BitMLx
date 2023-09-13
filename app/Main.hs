module Main where

import Prettyprinter.Render.Text ( renderLazy ) 
import Prettyprinter.Internal
    ( layoutPretty, defaultLayoutOptions )
import Data.Text.Lazy.IO as TL ( writeFile )

import Compiler ( compile )
import Pretty ( prettyprintNL )
import Syntax.BitML (ContractAdvertisement(..))

import Examples.SimpleExchange
    ( exampleName, participants, sourceAdvertisement)

main :: IO ()
main = do
    case compile sourceAdvertisement of
        Right (ContractAdvertisement (bitcoinPreconditions, bitcoinContract), ContractAdvertisement (dogecoinPreconditions, dogecoinContract)) -> do
            let            
                bitcoinOutPath = "output/" ++ exampleName ++ "_bitcoin.rkt"
                dogecoinOutPath = "output/" ++ exampleName ++ "_dogecoin.rkt"

                bitcoinDoc = prettyprintNL participants bitcoinPreconditions bitcoinContract 
                dogecoinDoc = prettyprintNL participants dogecoinPreconditions dogecoinContract
                renderBitcoin = renderLazy (layoutPretty defaultLayoutOptions bitcoinDoc)
                renderDogecoin = renderLazy (layoutPretty defaultLayoutOptions dogecoinDoc)
            TL.writeFile bitcoinOutPath renderBitcoin
            TL.writeFile dogecoinOutPath renderDogecoin
        Left compilationError -> print $ show compilationError
