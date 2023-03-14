#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 1 "dd_a")
  (deposit "A" 2 "dc_a")
  (deposit "B" 2 "dc_b")
  )

 (split
   (1 -> (withdraw "B")) (2 -> (withdraw "A")) (2 -> (withdraw "B")))
 )
