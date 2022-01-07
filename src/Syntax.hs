module Syntax where

--      BitMLx Syntax
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
data GCx = Advx P Gx Cx

--- end of BitMLx syntax ---


---     BitML// syntax

-- contract preconditions
data G  = Dep Pname V X
        | VolDep Pname V X
        | Secret Pname Sname Shash
        | SecG Gx Gx
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

-- contract
data C  = Solo D
        | Choice C C            -- it could not be defined as Choice D D, bc if that was the case we could not have more than 2 choices each time
        deriving (Eq,Show)

 -- contract advertisement
data GC = Adv P G C
       