#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 1 "A_deposit_Bitcoin")
  (deposit "B" 0 "B_deposit_Bitcoin")
  (secret "A" StepSecret_A__L_ "__HASH__PLACEHOLDER__")
  (secret "B" StepSecret_B__L_ "__HASH__PLACEHOLDER__")
  )

 (choice
  (reveal (StepSecret_A__L_) (withdraw "B"))
  (reveal (StepSecret_B__L_) (withdraw "B"))
  (after 11 (reveal () (choice
                        (reveal (StepSecret_A__L_) (withdraw "B"))
                        (reveal (StepSecret_B__L_) (withdraw "A"))
                        (after 21 (reveal () (withdraw "A"))
                         )
                        ))
   )
  ))
