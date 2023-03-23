{-|
Module      : Pretty
Description : Pretty printer for BitML contracts.

This functions take a contract on our Haskell representation of BitML and
print them to a racket program compatible with the style of Bartoletti and
Zunino'simplementation.
-}

module Pretty where

import Prettyprinter.Internal
    ( (<+>), align, emptyDoc, hsep, line, sep, Doc, Pretty(pretty) )
import Prettyprinter.Internal.Type ( Doc )
import Prettyprinter.Symbols.Ascii ( dquotes, parens )

import Syntax.Common ( E(..), P(P, pname), Pred(..) )
import Syntax.BitML
    ( D(PutRevealIf, Withdraw, Split, Auth, After, Reveal, Put,
        RevealIf, PutReveal),
      C,
      G(..) )
import Coins (Coins, BCoins(..), DCoins(..))



instance Pretty BCoins where
  pretty (BCoins coins) = pretty coins

instance Pretty DCoins where
  pretty (DCoins coins) = pretty coins

prettypred :: Pred -> Doc x
prettypred PTrue           = pretty "true"
prettypred (PAnd p1 p2)    = parens $ pretty "and" <+> prettypred p1 <+> prettypred p2
prettypred (POr p1 p2)     = parens $ pretty "or" <+> prettypred p1 <+> prettypred p2
prettypred (PNot p)        = parens $ pretty "not" <+> prettypred p
prettypred (PEq e1 e2)     = parens $ pretty "=" <+> prettyexp e1 <+> prettyexp e2
prettypred (PNeq e1 e2)    = parens $ pretty "!=" <+> prettyexp e1 <+> prettyexp e2
prettypred (PBtwn p e1 e2) = parens $ pretty "between" <+> prettyexp p <+> prettyexp e1 <+> prettyexp e2
prettypred (PLt e1 e2)     = parens $ pretty "<" <+> prettyexp e1 <+> prettyexp e2

prettypred' :: Pred -> Doc x
prettypred' p = parens $ pretty "pred" <+> prettypred p

prettyexp :: E -> Doc x
prettyexp (EInt n)      = pretty n
prettyexp (ELength s)   = pretty s   -- usually a name of a secret to measure its length
prettyexp (EAdd e1 e2)  = parens $ pretty "+" <+> prettyexp e1 <+> prettyexp e2
prettyexp (ESub e1 e2)  = parens $ pretty "-" <+> prettyexp e1 <+> prettyexp e2

prettyprintSplit :: [Doc a] -> [Doc a] -> Doc a
prettyprintSplit xs cs =  align ( pretty "split" <> line)  <+> ( align $ sep $ zipWith ( \x y -> parens (x <+> (pretty "->") <+> y) ) xs cs)


prettyprint :: (Coins v, Pretty v) => C v -> Doc x
prettyprint []                             = emptyDoc
prettyprint (Withdraw p : cs)              = parens ( align ( pretty "withdraw" <+> dquotes (pretty (pname p))) ) <> align (prettyprint cs ) -- <> line
prettyprint (Split cs : cs')            = parens ( prettyprintSplit (map (pretty . fst) cs) (map (prettyprintNew . snd) cs) ) <> line <> align  (prettyprint cs')  -- <> line
prettyprint (Auth ps d : cs)                = parens ( align (pretty "auth" <+>  hsep (map (dquotes . pretty . pname) ps) <+> prettyprint [d] ) ) <> line <>  align (prettyprint cs ) -- <> line
prettyprint (After t d : cs)               = parens ( align (pretty "after" <+> pretty t <+> prettyprint [d]) ) <> line <> align (prettyprint cs )
prettyprint (Reveal as cs : cs')           = parens ( align (pretty "reveal" <+> parens (hsep (map pretty as) ) <+> align ( prettyprintNew cs) ) ) <> line <> align (prettyprint cs' )
prettyprint (Put xs cs : cs')              = parens ( align (pretty "put" <+> hsep (map pretty xs) <+> prettyprintNew cs ) ) <> line <> align (prettyprint cs' )
prettyprint (RevealIf as pred cs : cs')    = parens ( align (pretty "revealif" <+> parens (hsep (map pretty as)) <+> prettypred' pred <+> prettyprintNew cs ) ) <> line <> align (prettyprint cs' )
prettyprint (PutReveal xs as cs : cs')        = parens ( align (pretty "putrevealif" <+> parens (hsep (map pretty xs) )<+> parens (hsep (map pretty as) ) <+> align ( prettyprintNew cs) ) ) <> line <> align (prettyprint cs' )
prettyprint (PutRevealIf xs as pred cs : cs') = parens ( align (pretty "putrevealif" <+> parens (hsep (map pretty xs)) <+> parens (hsep (map pretty as)) <+> prettypred' pred <+> prettyprintNew cs ) ) <> line <> align (prettyprint cs' )

prettyprintNew :: (Coins v, Pretty v) => C v -> Doc x
prettyprintNew []   = emptyDoc
prettyprintNew [c]  = prettyprint [c]
prettyprintNew c    = parens $ pretty "choice" <> line <+> align (prettyprint c)


prettyprintG :: (Coins v, Pretty v) => [G v] -> Doc x
prettyprintG                      [] = emptyDoc
prettyprintG (Deposit p v tx : gs)       = parens ( align (pretty "deposit" <+> dquotes (pretty (pname p)) <+> (pretty v) <+> dquotes (pretty tx) ) ) <> line <> align (prettyprintG gs)
prettyprintG (Volatile p v x tx : gs)  = parens ( align (pretty "vol-deposit" <+> dquotes (pretty (pname p)) <+> (pretty x ) <+> (pretty v) <+> dquotes (pretty tx) ) ) <> line <> align (prettyprintG gs)
prettyprintG (Secret p s shash : gs) = parens ( align (pretty "secret"<+> dquotes (pretty (pname p)) <+> (pretty s) <+> dquotes (pretty shash) ) ) <> line <> align (prettyprintG gs)

prettyprintGNew :: (Coins v, Pretty v) => [G v] -> Doc x
prettyprintGNew [] = emptyDoc
prettyprintGNew g  = parens $ pretty "pre" <> line <+> align (prettyprintG g)

prettyprintP :: [P] -> Doc x
prettyprintP              [] = emptyDoc
prettyprintP (P p pk : ps) = parens (align (pretty "participant" <+> dquotes (pretty p) <+> dquotes (pretty pk) ) ) <> line <> align (prettyprintP ps)

prettyprintGC :: (Coins v, Pretty v) => [G v] -> C v-> Doc x
prettyprintGC g c = parens ( pretty "contract" <> line <+> align ( prettyprintGNew g ) <> line <> line <+> align ( prettyprintNew c )) <> line

prettyprintPGC :: (Coins v, Pretty v) => [P] -> [G v] -> C v-> Doc x
prettyprintPGC p g c = prettyprintP p <> line <> prettyprintGC g c

prettyprintNL :: (Coins v, Pretty v) => [P] -> [G v] -> C v -> Doc x
prettyprintNL p g c = pretty "#lang bitml" <> line <> line <> pretty "(debug-mode)" <> line <> line <> prettyprintPGC p g c
