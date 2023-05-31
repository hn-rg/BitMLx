#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 0 "dd_a")
  (deposit "B" 1 "dd_b")
  (deposit "A" 0 "dc_a")
  (deposit "B" 0 "dc_b")
  (secret "A" A_Dogecoin_S_Name_L_ "__SOME_HASH__")
  (secret "B" B_Dogecoin_S_Name_L_ "__SOME_HASH__")
  )

 (choice
  (reveal (A_Dogecoin_S_Name_L_) (split
                                   (1 -> (withdraw "A")) (0 -> (withdraw "B")))
                                 )
  (reveal (B_Dogecoin_S_Name_L_) (split
                                   (1 -> (withdraw "A")) (0 -> (withdraw "B")))
                                 )
  (after 11 (reveal () (choice
                        (reveal (A_Bitcoin_S_Name_L_) (withdraw "B"))
                        (reveal (B_Bitcoin_S_Name_L_) (withdraw "A"))
                        (after 21 (reveal () (split
                                               (0 -> (withdraw "A"))
                                               (1 -> (withdraw "B")))
                                             )
                         )
                        ))
   )
  ))
