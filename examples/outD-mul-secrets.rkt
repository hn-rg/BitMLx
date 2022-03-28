#lang bitml

(debug-mode)

(participant "A" "txA")
(participant "B" "txB")

(contract
 (pre
  (deposit "A" 1 "x1")
  (deposit "A" 1 "x1col")
  (secret "A" a "aaa")
  (secret "A" b "bbb")
  (secret "A" s1Ab "001Ab")
  (secret "A" s2Ab "002Ab")
  (secret "A" s1Ad "001Ad")
  (secret "A" s2Ad "002Ad")
  (deposit "B" 1 "y1col")
  (secret "B" s1Bb "001Bb")
  (secret "B" s2Bb "002Bb")
  (secret "B" s1Bd "001Bd")
  (secret "B" s2Bd "002Bd")
  )

 (choice
  (revealif (s1Ad a b) (pred (not (< a b))) (split
                                              (1 -> (withdraw "A"))
                                              (2 -> (withdraw "A")))
   )
  (revealif (s1Bd a b) (pred (not (< a b))) (split
                                              (1 -> (withdraw "A"))
                                              (2 -> (withdraw "A")))
   )
  (after 1 (reveal (s1Ab) (split
                            )
                          )
   )
  (after 1 (reveal (s1Bb) (split
                            (3 -> (withdraw "A")))
                          )
   )
  (after 11 (revealif (s2Ad a b) (pred (not (< (- b 1) b))) (split
                                                              (1 -> (withdraw "A"))
                                                              (2 -> (withdraw "A")))
             )
   )
  (after 11 (revealif (s2Bd a b) (pred (not (< (- b 1) b))) (split
                                                              (1 -> (withdraw "A"))
                                                              (2 -> (withdraw "A")))
             )
   )
  (after 11 (after 22 (reveal (s2Ab) (split
                                       )
                                     )
             )
   )
  (after 11 (after 22 (reveal (s2Bb) (split
                                       (3 -> (withdraw "A")))
                                     )
             )
   )
  ))
