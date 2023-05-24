#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 2 "bd_a")
  (deposit "B" 2 "bd_b")
  (deposit "A" 0 "bc_a")
  (deposit "B" 0 "bc_b")
  (secret "A" A_Bitcoin_S_Name__ "A_Bitcoin_S_Hash__")
  (secret "B" B_Bitcoin_S_Name__ "B_Bitcoin_S_Hash__")
  (secret "A" A_Bitcoin_S_Name_L_0 "A_Bitcoin_S_Hash_L_0")
  (secret "B" B_Bitcoin_S_Name_L_0 "B_Bitcoin_S_Hash_L_0")
  (secret "A" A_Bitcoin_S_Name_L_1 "A_Bitcoin_S_Hash_L_1")
  (secret "B" B_Bitcoin_S_Name_L_1 "B_Bitcoin_S_Hash_L_1")
  )

 (choice
  (reveal (A_Bitcoin_S_Name__) (split
                                 (2 -> (choice
                                  (reveal (A_Bitcoin_S_Name_L_0) (withdraw "A"))
                                  (reveal (B_Bitcoin_S_Name_L_0) (withdraw "A"))
                                  (after 21 (reveal () (choice
                                                        (reveal (A_Dogecoin_S_Name_L_0) (withdraw "B"))
                                                        (reveal (B_Dogecoin_S_Name_L_0) (withdraw "A"))
                                                        ))
                                   )
                                  ))
                                 (2 -> (choice
                                  (reveal (A_Bitcoin_S_Name_L_1) (withdraw "B"))
                                  (reveal (B_Bitcoin_S_Name_L_1) (withdraw "B"))
                                  (after 21 (reveal () (choice
                                                        (reveal (A_Dogecoin_S_Name_L_1) (withdraw "B"))
                                                        (reveal (B_Dogecoin_S_Name_L_1) (withdraw "A"))
                                                        ))
                                   )
                                  )))
                               )
  (reveal (B_Bitcoin_S_Name__) (split
                                 (2 -> (choice
                                  (reveal (A_Bitcoin_S_Name_L_0) (withdraw "A"))
                                  (reveal (B_Bitcoin_S_Name_L_0) (withdraw "A"))
                                  (after 21 (reveal () (choice
                                                        (reveal (A_Dogecoin_S_Name_L_0) (withdraw "B"))
                                                        (reveal (B_Dogecoin_S_Name_L_0) (withdraw "A"))
                                                        ))
                                   )
                                  ))
                                 (2 -> (choice
                                  (reveal (A_Bitcoin_S_Name_L_1) (withdraw "B"))
                                  (reveal (B_Bitcoin_S_Name_L_1) (withdraw "B"))
                                  (after 21 (reveal () (choice
                                                        (reveal (A_Dogecoin_S_Name_L_1) (withdraw "B"))
                                                        (reveal (B_Dogecoin_S_Name_L_1) (withdraw "A"))
                                                        ))
                                   )
                                  )))
                               )
  (after 11 (reveal () (choice
                        (reveal (A_Dogecoin_S_Name__) (withdraw "B"))
                        (reveal (B_Dogecoin_S_Name__) (withdraw "A"))
                        ))
   )
  ))
