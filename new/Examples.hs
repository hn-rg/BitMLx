module Examples where

import Syntax    

    -- example 1 -> ZD-lottery
p1 :: Pl
p1 = [  Par "A" "txA" ,
        Par "B" "txB"  
     ]
g1 :: Gxl
g1 = [  Depx "A" (1, 1) ("x1", "x2"),
        Secretx "A" "a" "000a",
        Depx "B" (1, 1) ("y1", "y2"),
        Secretx "B" "b" "000b",
        DepCol "A" (2,2) ("x1col", "x2col"),
        SecretPlusB "A" [ ("s1Ab", "001Ab"), ("s2Ab", "002Ab"), ("s3Ab", "003Ab"), ("s4Ab", "004Ab"), ("s5Ab", "005Ab") ] ,
        SecretPlusD "A" [ ("s1Ad", "001Ad"), ("s2Ad", "002Ad"), ("s3Ad", "003Ad"), ("s4Ad", "004Ad"), ("s5Ad", "005Ad")  ] ,
        DepCol "B" (2,2) ("y1col","y2col"), 
        SecretPlusB "B" [ ("s1Bb", "001Bb"), ("s2Bb", "002Bb"), ("s3Bb", "003Bb"), ("s4Bb", "004Bb"), ("s5Bb", "005Bb")  ] ,
        SecretPlusD "B" [ ("s1Bd", "001Bd"), ("s2Bd", "002Bd"), ("s3Bd", "003Bd"), ("s4Bd", "004Bd"), ("s5Bd", "005Bd")  ] 
    ]

c1 :: Cx
c1 =  [ Revealifx ["b"] (Pbtwn (Elength "b") (Eint 0) (Eint 1)) [(Revealifx ["a","b"] (Peq (Elength "a") (Elength "b")) [Withdrawx "A"])]
      , Revealifx ["b"] (Pbtwn (Elength "b") (Eint 0) (Eint 1)) [(Revealifx ["a","b"] (Pneq (Elength "a") (Elength "b")) [Withdrawx "B"])]
      , Revealifx ["b"] (Pbtwn (Elength "b") (Eint 0) (Eint 1)) [(Withdrawx "B")]
      , Withdrawx "A"    -- "a" and "b" are names not values!
      ]  

adv :: GCx
adv = Advx p1 g1 c1
-- end of example 1

-- example 2 -> 2p-mutual-tc
p2 :: Pl
p2 = [  Par "A" "txA" ,
        Par "B" "txB"  
     ]
g2 :: Gxl
g2 = [  Depx "A" (1, 1) ("x1", "x2"),
        Secretx "A" "a" "000a",
        Depx "B" (1, 1) ("y1", "y2"),
        Secretx "B" "b" "000b",
        DepCol "A" (2,2) ("x1col", "x2col"),
        SecretPlusB "A" [ ("s1Ab", "001Ab"), ("s2Ab", "002Ab"), ("s3Ab", "003Ab"), ("s4Ab", "004Ab")] ,
        SecretPlusD "A" [ ("s1Ad", "001Ad"), ("s2Ad", "002Ad"), ("s3Ad", "003Ad"), ("s4Ad", "004Ad")  ] ,
        DepCol "B" (2,2) ("y1col","y2col"), 
        SecretPlusB "B" [ ("s1Bb", "001Bb"), ("s2Bb", "002Bb"), ("s3Bb", "003Bb"), ("s4Bb", "004Bb")  ] ,
        SecretPlusD "B" [ ("s1Bd", "001Bd"), ("s2Bd", "002Bd"), ("s3Bd", "003Bd"), ("s4Bd", "004Bd")  ] 
    ]

c2 :: Cx
c2 = [ Revealx ["a"] [Revealx ["b"] [Splitx [(1,1), (1,1)] [[Withdrawx "A"], [Withdrawx "B"]] ] ]
     , Revealx ["a" ][Withdrawx "A"]
     , Revealx ["b"] [Withdrawx "B"]
     , Splitx [(1,1), (1,1)] [[Withdrawx "A"], [Withdrawx "B"]]
    ]
-- end of example 2

-- example 3 -> multiple secrets
p3 :: Pl
p3 = p1

g3 :: Gxl
g3 = [  Depx "A" (1, 1) ("x1", "x2"),
        DepCol "A" (1,1) ("x1col", "x2col"),
        Secretx "A" "a" "aaa",
        Secretx "A" "b" "bbb",
        SecretPlusB "A" [ ("s1Ab", "001Ab"), ("s2Ab", "002Ab")  ] ,
        SecretPlusD "A" [ ("s1Ad", "001Ad"), ("s2Ad", "002Ad")  ] ,
        DepCol "B" (1,1) ("y1col","y2col"), 
        SecretPlusB "B" [ ("s1Bb", "001Bb"), ("s2Bb", "002Bb") ] ,
        SecretPlusD "B" [ ("s1Bd", "001Bd"), ("s2Bd", "002Bd") ] 
    ]

c3 :: Cx
c3 = [ Revealifx ["a", "b"] (Pnot (Plt (Elength "a") (Elength "b")) ) [Withdrawx "A"] 
     , Revealifx ["a", "b"] (Pnot (Plt (Esub (Elength "b") (Eint 1)) (Elength "b")) ) [Withdrawx "A"] 
     ]
-- end of example 3

-- example 4 -> put
p4 :: Pl
p4 = p1

g4 :: Gxl
g4 = [  Depx "A" (1, 1) ("x1", "x2"),
        DepCol "A" (1,1) ("x1col", "x2col"),
        SecretPlusB "A" [ ("s1Ab", "001Ab") ] ,
        SecretPlusD "A" [ ("s1Ad", "001Ad")  ] ,
        VolDepx "B" (1,1) ("x1","x2") ("tx1", "tx2"),
        VolDepx "B" (1,1) ("y1","y2") ("txy1", "txy2"),
        DepCol "B" (1,1) ("y1col","y2col"), 
        SecretPlusB "B" [ ("s1Bb", "001Bb") ] ,
        SecretPlusD "B" [ ("s1Bd", "001Bd") ] 
    ]

c4 :: Cx
c4 = [ Putx [("x1","x2"), ("y1","y2")] [Withdrawx "A"]
     ]


