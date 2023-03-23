#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 1 "bd_a")
  (deposit "B" 1 "bd_b")
  (deposit "A" 2 "bc_a")
  (deposit "B" 2 "bc_b")
  (secret "A" A_Bitcoin_S_Name "A_Bitcoin_S_Hash")
  (secret "B" B_Bitcoin_S_Name "B_Bitcoin_S_Hash")
  )

 (choice
  (reveal (A_Bitcoin_S_Name) (split
                               (2 -> (withdraw "A")) (0 -> (withdraw "B")))
                             )
  (reveal (B_Bitcoin_S_Name) (split
                               (2 -> (withdraw "A")) (0 -> (withdraw "B")))
                             )
  (after 11 (reveal () (choice
                        (reveal (A_Dogecoin_S_Name) (split
                                                      (2 -> (withdraw "B")))
                                                    )
                        (reveal (B_Dogecoin_S_Name) (split
                                                      (2 -> (withdraw "A")))
                                                    )
                        ))
   )
  ))
