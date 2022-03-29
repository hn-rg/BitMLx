#lang bitml

(debug-mode)

(participant "A" "txA")
(participant "B" "txB")

(contract
 (pre
  (deposit "A" 1 "x1")
  (secret "A" a "000a")
  (deposit "B" 1 "y1")
  (secret "B" b "000b")
  (deposit "A" 2 "x1col")
  (secret "A" s1Ab "001Ab")
  (secret "A" s2Ab "002Ab")
  (secret "A" s3Ab "003Ab")
  (secret "A" s4Ab "004Ab")
  (secret "A" s1Ad "001Ad")
  (secret "A" s2Ad "002Ad")
  (secret "A" s3Ad "003Ad")
  (secret "A" s4Ad "004Ad")
  (deposit "B" 2 "y1col")
  (secret "B" s1Bb "001Bb")
  (secret "B" s2Bb "002Bb")
  (secret "B" s3Bb "003Bb")
  (secret "B" s4Bb "004Bb")
  (secret "B" s1Bd "001Bd")
  (secret "B" s2Bd "002Bd")
  (secret "B" s3Bd "003Bd")
  (secret "B" s4Bd "004Bd")
  )

 (choice
  (reveal (s1Ad a) (choice
                    (reveal (s2Ad b) (split
                                       (3 -> (split
                                               (1 -> (withdraw "A"))
                                               (2 -> (withdraw "A")))
                                       )
                                       (3 -> (split
                                               (1 -> (withdraw "B"))
                                               (2 -> (withdraw "A")))
                                       ))
                                     )
                    (reveal (s2Bd b) (split
                                       (3 -> (split
                                               (1 -> (withdraw "A"))
                                               (2 -> (withdraw "A")))
                                       )
                                       (3 -> (split
                                               (1 -> (withdraw "B"))
                                               (2 -> (withdraw "A")))
                                       ))
                                     )
                    (after 2 (reveal (s2Ab) (split
                                              (5 -> (withdraw "B")))
                                            )
                     )
                    (after 2 (reveal (s2Bb) (split
                                              (5 -> (withdraw "A")))
                                            )
                     )
                    ))
  (reveal (s1Bd a) (choice
                    (reveal (s2Ad b) (split
                                       (3 -> (split
                                               (1 -> (withdraw "A"))
                                               (2 -> (withdraw "A")))
                                       )
                                       (3 -> (split
                                               (1 -> (withdraw "B"))
                                               (2 -> (withdraw "A")))
                                       ))
                                     )
                    (reveal (s2Bd b) (split
                                       (3 -> (split
                                               (1 -> (withdraw "A"))
                                               (2 -> (withdraw "A")))
                                       )
                                       (3 -> (split
                                               (1 -> (withdraw "B"))
                                               (2 -> (withdraw "A")))
                                       ))
                                     )
                    (after 2 (reveal (s2Ab) (split
                                              (5 -> (withdraw "B")))
                                            )
                     )
                    (after 2 (reveal (s2Bb) (split
                                              (5 -> (withdraw "A")))
                                            )
                     )
                    ))
  (after 1 (reveal (s1Ab) (split
                            (5 -> (withdraw "B")))
                          )
   )
  (after 1 (reveal (s1Bb) (split
                            (5 -> (withdraw "A")))
                          )
   )
  (after 11 (reveal (s3Ad a) (split
                               (2 -> (withdraw "A"))
                               (2 -> (withdraw "A"))
                               (2 -> (withdraw "B")))
                             )
   )
  (after 11 (reveal (s3Bd a) (split
                               (2 -> (withdraw "A"))
                               (2 -> (withdraw "A"))
                               (2 -> (withdraw "B")))
                             )
   )
  (after 11 (after 33 (reveal (s3Ab) (split
                                       (5 -> (withdraw "B")))
                                     )
             )
   )
  (after 11 (after 33 (reveal (s3Bb) (split
                                       (5 -> (withdraw "A")))
                                     )
             )
   )
  (after 11 (after 43 (reveal (s4Ad b) (split
                                         (2 -> (withdraw "B"))
                                         (2 -> (withdraw "A"))
                                         (2 -> (withdraw "B")))
                                       )
             )
   )
  (after 11 (after 43 (reveal (s4Bd b) (split
                                         (2 -> (withdraw "B"))
                                         (2 -> (withdraw "A"))
                                         (2 -> (withdraw "B")))
                                       )
             )
   )
  (after 11 (after 43 (after 172 (reveal (s4Ab) (split
                                                  (5 -> (withdraw "B")))
                                                )
                       )
             )
   )
  (after 11 (after 43 (after 172 (reveal (s4Bb) (split
                                                  (5 -> (withdraw "A")))
                                                )
                       )
             )
   )
  (after 11 (after 43 (after 182 (split
                                   (3 -> (split
                                           (1 -> (withdraw "A"))
                                           (2 -> (withdraw "A")))
                                   )
                                   (3 -> (split
                                           (1 -> (withdraw "B"))
                                           (2 -> (withdraw "A")))
                                   ))
                       )
             )
   )
  ))