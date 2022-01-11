module Examples where

import Syntax    

    -- example 1
p1 :: P
p1 = SecPar
        (Par "A" "029c5f6f5ef0095f547799cb7861488b9f4282140d59a6289fbc90c70209c1cced")
        (Par "B" "022c3afb0b654d3c2b0e2ffdcf941eaf9b6c2f6fcf14672f86f7647fa7b817af30")
g1 :: Gx
g1 = SecGx (SecGx (Depx "A" (1,1) ("x1", "x2"))
                (Secretx "A" "a" "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b"))
           (SecGx (Depx "B" (1,1) ("y1", "y2"))
                (Secretx "B" "b" "d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35"))

g2 :: Gx
g2 = SecGx
        ( SecGx
                (DepCol "A" (2,2) ("x1col","x2col"))                    -- give collaterals
                (SecGx
                        (SecretPlusB "A" [("s1Ab", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875thg")] )   -- A's extra secret for bitcoin
                        (SecretPlusD "A" [("s1Ad", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875hjk")] )   -- A's extra secret for bitcoin
                )
        )
        ( SecGx
                (DepCol "B" (2,2) ("y1col","y2col"))
                ( SecGx
                        (SecretPlusB "B" [("s1Bb", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7859133")] )
                        (SecretPlusD "B" [("s1Bd", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7859567")] )
                )
        )

g :: Gx
g = SecGx g1 g2

d1 :: Dx
d1 = Revealifx ["a","b"] (Peq (Elength "a") (Elength "a") )  (Solox (Withdrawx "A"))

d2 :: Dx
d2 = Revealifx ["a","b"] (Pnot (Peq (Elength "a") (Elength "b")) )  (Solox (Withdrawx "B"))  -- "a" and "b" are names not values!

c :: Cx
c = PriChoice (Solox d1) (Solox d2)

adv :: GCx
adv = Advx p1 g c

cSimpleTest :: Cx
cSimpleTest = PriChoice (Solox (Withdrawx "A")) (Solox (Withdrawx "B"))
-- end of example 1