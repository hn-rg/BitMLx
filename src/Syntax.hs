module Syntax where

--      Syntax
type Pname = String

type PK = String

type Vb = Int

type Vd = Int

type Xb = String

type Xd = String

type Sname = String

type Shash = String

type Level = Int

-- predicate
data Pred   = PTrue
            | Pand Pred Pred
            | Pnot Pred
            | Peq E E
            | Plt E E
        deriving (Eq,Show)

-- arithmetic expression
data E  = Eint Integer
        | Elength Sname      -- length of a secret
        | Eadd E E
        | Esub E E
        deriving (Eq,Show)

-- participants in contract
data P =  Par Pname PK
        | SecPar P P
        deriving (Eq,Show)

-- contract preconditions
data Gx = Depx Pname (Vb,Vd) (Xb,Xd)
        | VolDepx Pname (Vb,Vd) (Xb,Xd)
        | Secretx Pname Sname Shash
        | DepCol Pname (Vb,Vd) (Xb,Xd)
        | SecretPlus Pname [(Sname, Shash)]
        | SecGx Gx Gx
        deriving (Eq,Show)

-- guarded contract
data Dx = Putx [(Xb,Xd)] Cx
        | Revealx [Sname] Cx
        | Revealifx [Sname] Pred Cx
        | PutRevx [(Xb,Xd)] [Sname] Cx
        | PutRevifx [(Xb,Xd)] [Sname] Pred Cx
        | Withdrawx Pname
        | Authx Pname Dx
        | Splitx [(Vb,Vd)] [Cx]
        deriving (Eq,Show)

-- contract
data Cx = Solox Dx
        | PriChoice Cx Cx
        deriving (Eq,Show)

 -- contract advertisement
data GCx = Adv P Gx Cx