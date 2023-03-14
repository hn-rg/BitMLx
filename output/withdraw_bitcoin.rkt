#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 1 "bd_a")
  (deposit "A" 2 "bc_a")
  (deposit "B" 2 "bc_b")
  )

 (split
   (1 -> (withdraw "B")) (2 -> (withdraw "A")) (2 -> (withdraw "B")))
 )
