#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 2 "dd_a")
  (deposit "B" 2 "dd_b")
  (deposit "A" 0 "dc_a")
  (deposit "B" 0 "dc_b")
  (secret "A" A_Dogecoin_S_Name_L_ "A_Dogecoin_S_Hash_L_")
  (secret "B" B_Dogecoin_S_Name_L_ "B_Dogecoin_S_Hash_L_")
  )

 (choice
  (reveal (A_Dogecoin_S_Name_L_) (split
                                   (2 -> (withdraw "A")) (2 -> (withdraw "B")))
                                 )
  (reveal (B_Dogecoin_S_Name_L_) (split
                                   (2 -> (withdraw "A")) (2 -> (withdraw "B")))
                                 )
  (after 11 (reveal () (choice
                        (reveal (A_Bitcoin_S_Name_L_) (withdraw "B"))
                        (reveal (B_Bitcoin_S_Name_L_) (withdraw "A"))
                        (after 21 (reveal () (split
                                               (2 -> (withdraw "A"))
                                               (2 -> (withdraw "B")))
                                             )
                         )
                        ))
   )
  ))
