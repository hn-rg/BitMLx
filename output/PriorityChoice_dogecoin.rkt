#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 1 "dd_a")
  (deposit "B" 1 "dd_b")
  (deposit "A" 2 "dc_a")
  (deposit "B" 2 "dc_b")
  (secret "A" A_Dogecoin_S_Name "A_Dogecoin_S_Hash")
  (secret "B" B_Dogecoin_S_Name "B_Dogecoin_S_Hash")
  )

 (choice
  (reveal (A_Dogecoin_S_Name) (split
                                (2 -> (withdraw "A")) (0 -> (withdraw "B")))
                              )
  (reveal (B_Dogecoin_S_Name) (split
                                (2 -> (withdraw "A")) (0 -> (withdraw "B")))
                              )
  (after 11 (reveal () (choice
                        (reveal (A_Bitcoin_S_Name) (split
                                                     (2 -> (withdraw "B")))
                                                   )
                        (reveal (B_Bitcoin_S_Name) (split
                                                     (2 -> (withdraw "A")))
                                                   )
                        ))
   )
  ))
