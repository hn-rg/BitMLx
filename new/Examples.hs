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
        SecretPlusB "A" [ ("s1Ab", "hi1"), ("s2Ab", "hi2"),  ("s3Ab", "hi3") ] ,
        SecretPlusD "A" [ ("s1Ad", "hi1x"), ("s2Ad", "hi2x") , ("s3Ad", "hi3x")  ] ,
        DepCol "B" (2,2) ("y1col","y2col"), 
        SecretPlusB "B" [ ("s1Bb", "hii1"), ("s2Bb", "hii2"), ("s3Bb", "hii3") ] ,
        SecretPlusD "B" [ ("s1Bd", "hii1x"), ("s2Bd", "hii2x") , ("s3Bd", "hii3x") ] , 
        VolDepx "A" (1,1) ("x1","x2"),
        VolDepx "B" (1,1) ("y1","y2") 
    ]



c1 :: Cx
c1 =  [ Authx "A" $ Revealifx ["a","b"] (Peq (Elength "a") (Elength "a") )  [Withdrawx "A"] ,
        Revealifx ["a","b"] (Pnot (Peq (Elength "a") (Elength "b")) )  [ Withdrawx "B" ]   -- "a" and "b" are names not values!
      ]  

adv :: GCx
adv = Advx p1 g1 c1

-- cSimpleTest :: Cx
--cSimpleTest = [Splitx [(1,1), (1,1)] [[Withdrawx "A"], [Withdrawx "B", Withdrawx "A"]], Authx "A" $ Withdrawx "B"]

cSimpleTest :: Cx
cSimpleTest = [Putx [("x1","x2"), ("y1","y2")] [Withdrawx "A"], Authx "A" $ Revealifx ["a","b"] (Peq (Elength "a") (Elength "a") )  [Withdrawx "A"]  ]
-- end of example 1