module Syntax where

type Pname = String

type PK = String

type Vb = Int

type Vd = Int

type V  = Int

type Xb = String

type Xd = String

type X  = String

type Sname = String

type Shash = String

type Level = Int

type Time = Int

-- | should be: Authx [Pname] Dx
-- instead of Authx Pname Dx

-- |     BitMLx Syntax

-- list participants in contract
type Pl = [P]

-- list of contract preconditions
type Gxl = [Gx]

-- contract
type Cx = [Dx]


-- predicate
data Pred   = PTrue
            | Pand Pred Pred
            | Por Pred Pred
            | Pnot Pred
            | Peq E E
            | Pneq E E
            | Pbtwn Pred E E
            | Plt E E
        deriving (Eq, Ord, Show)

-- arithmetic expression
data E  = Eint Integer
        | Elength Sname      -- length of a secret
        | Eadd E E
        | Esub E E
        deriving (Eq, Ord, Show)

-- participant in contract
data P =  Par Pname PK
        --deriving (Eq,Show)

-- contract preconditions
data Gx = Depx Pname (Vb,Vd) (Xb,Xd)                    -- deposit participant (bitcoin, dogecoin) (tx-name, tx-name)
        | VolDepx Pname (Vb,Vd) (Xb,Xd) (Xb,Xd)         -- vol-deposit participant (bitcoin, dogecoin) (dep-name, dep-name) (tx-name, tx-name)
        | Secretx Pname Sname Shash
        | DepCol Pname (Vb,Vd) (Xb,Xd)
        | SecretPlusB Pname [(Sname, Shash)]
        | SecretPlusD Pname [(Sname, Shash)]
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
        deriving (Eq, Ord, Show)


 -- contract advertisement
data GCx = Advx Pl Gxl Cx

--- end of BitMLx syntax ---


---     BitML// syntax

-- list of contract preconditions
type Gl = [G]

type C = [D]

-- contract preconditions
data G  = Dep Pname V X
        | VolDep Pname V X X
        | Secret Pname Sname Shash
        deriving (Eq,Show)

-- guarded contract
data D  = Put [X] C
        | Reveal [Sname] C
        | Revealif [Sname] Pred C
        | PutRev [X] [Sname] C
        | PutRevif [X] [Sname] Pred C
        | Withdraw Pname
        | Auth Pname D
        | Split [V] [C]
        | After Time D
       deriving (Eq,Show)

 -- contract advertisement
data GC = Adv Pl Gl C
       