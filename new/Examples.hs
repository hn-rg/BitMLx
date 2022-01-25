module Examples where

import Syntax    

    -- example 1
p1 :: Pl
p1 = [  Par "A" "029c5f6f5ef0095f547799cb7861488b9f4282140d59a6289fbc90c70209c1cced" ,
        Par "B" "022c3afb0b654d3c2b0e2ffdcf941eaf9b6c2f6fcf14672f86f7647fa7b817af30"  
     ]
g1 :: Gxl
g1 = [  Depx "A" (1, 1) ("x1", "x2"),
        Secretx "A" "a" "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b",
        Depx "B" (1, 1) ("y1", "y2"),
        Secretx "B" "b" "d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35",
        DepCol "A" (2,2) ("x1col", "x2col"),
        SecretPlusB "A" [ ("s1Ab", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875thg") ] ,
        SecretPlusD "A" [ ("s1Ad", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875hjk")  ] ,
        DepCol "B" (2,2) ("y1col","y2col") ,
        SecretPlusB "B" [("s1Bb", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7859133") ] ,
        SecretPlusD "B" [("s1Bd", "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7859567") ] 
    ]



c1 :: Cx
c1 =  [ Revealifx ["a","b"] (Peq (Elength "a") (Elength "a") )  [ Withdrawx "B", Withdrawx "A", Withdrawx "B" ] ,
        Revealifx ["a","b"] (Pnot (Peq (Elength "a") (Elength "b")) )  [ Withdrawx "B", Withdrawx "A" ]   -- "a" and "b" are names not values!
      ]  

adv :: GCx
adv = Advx p1 g1 c1

cSimpleTest :: Cx
cSimpleTest = [Withdrawx "A", Withdrawx "B"]
-- end of example 1