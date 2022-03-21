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
            | Pnot Pred
            | Peq E E
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
data Gx = Depx Pname (Vb,Vd) (Xb,Xd)
        | VolDepx Pname (Vb,Vd) (Xb,Xd)
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
        | VolDep Pname V X
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
       