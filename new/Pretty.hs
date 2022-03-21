module Pretty where

import Syntax 

import Prettyprinter.Internal
import Prettyprinter.Internal.Type

import Prettyprinter.Util

prettyprin :: C -> Doc x
prettyprin [] = line
prettyprin [Withdraw p] = pretty "withdraw" <+> pretty p <+> line
prettyprin (Withdraw p : cs) = pretty "choice" <+> line <+> pretty "withdraw" <+> pretty p <+> line <+> align (prettyprin cs)
