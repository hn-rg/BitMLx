#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 0 "A_deposit_Dogecoin")
  (deposit "B" 1 "B_deposit_Dogecoin")
  (secret "A" StepSecret_A__L_ "__HASH__PLACEHOLDER__")
  (secret "B" StepSecret_B__L_ "__HASH__PLACEHOLDER__")
  )

 (choice
  (reveal (StepSecret_A__L_) (withdraw "A"))
  (reveal (StepSecret_B__L_) (withdraw "A"))
  (after 11 (reveal () (choice
                        (reveal (StepSecret_A__L_) (withdraw "B"))
                        (reveal (StepSecret_B__L_) (withdraw "A"))
                        (after 21 (reveal () (withdraw "B"))
                         )
                        ))
   )
  ))
