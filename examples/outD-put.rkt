#lang bitml

(debug-mode)

(participant "A" "txA")
(participant "B" "txB")

(contract
 (pre
  (deposit "A" 1 "x1")
  (deposit "A" 1 "x1col")
  (secret "A" s1Ab "001Ab")
  (secret "A" s1Ad "001Ad")
  (vol-deposit "B" x1 1 "tx1")
  (vol-deposit "B" y1 1 "txy1")
  (deposit "B" 1 "y1col")
  (secret "B" s1Bb "001Bb")
  (secret "B" s1Bd "001Bd")
  )

 (choice
  (putrevealif (x2 y2) (s1Ad) (split
                                (3 -> (withdraw "A")) (2 -> (withdraw "A")))
                              )
  (putrevealif (x2 y2) (s1Bd) (split
                                (3 -> (withdraw "A")) (2 -> (withdraw "A")))
                              )
  (after 1 (reveal (s1Ab) (split
                            )
                          )
   )
  (after 1 (reveal (s1Bb) (split
                            (3 -> (withdraw "A")))
                          )
   )
  ))
