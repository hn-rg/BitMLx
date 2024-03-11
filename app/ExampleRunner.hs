{-# LANGUAGE NamedFieldPuns #-}
module ExampleRunner where

import Prettyprinter.Render.Text ( renderLazy ) 
import Prettyprinter.Internal
    ( layoutPretty, defaultLayoutOptions )
import Data.Text.Lazy.IO as TL ( writeFile )
import Pretty ( prettyprintNL )

import Syntax.Common (P)
import Syntax.BitML as BitML
import Syntax.BitMLx as BitMLx ( ContractAdvertisement )
import Compiler ( compileBitMLx )


data BitMLxExample = BitMLxExample {
    exampleName :: String
    , participants :: [P]
    , sourceAdvertisement :: BitMLx.ContractAdvertisement
}


runExample :: BitMLxExample -> IO ()
runExample BitMLxExample {exampleName, participants, sourceAdvertisement} = case compileBitMLx sourceAdvertisement of
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