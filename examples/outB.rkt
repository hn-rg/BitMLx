#lang bitml

(debug-mode)

(participant "A" "029c5f6f5ef0095f547799cb7861488b9f4282140d59a6289fbc90c70209c1cced")
(participant "B" "022c3afb0b654d3c2b0e2ffdcf941eaf9b6c2f6fcf14672f86f7647fa7b817af30")

(contract
 (pre
  (deposit "A" 1 "x1")
  (secret "A" a "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b")
  (deposit "B" 1 "y1")
  (secret "B" b "d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35")
  (deposit "A" 2 "x1col")
  (secret "A" s1Ab "hi1")
  (secret "A" s2Ab "hi2")
  (secret "A" s3Ab "hi3")
  (secret "A" s1Ad "hi1x")
  (secret "A" s2Ad "hi2x")
  (secret "A" s3Ad "hi3x")
  (deposit "B" 2 "y1col")
  (secret "B" s1Bb "hii1")
  (secret "B" s2Bb "hii2")
  (secret "B" s3Bb "hii3")
  (secret "B" s1Bd "hii1x")
  (secret "B" s2Bd "hii2x")
  (secret "B" s3Bd "hii3x")
  (vol-deposit "A" x1 1 "txa1")
  (vol-deposit "B" y1 1 "txb1")
  )

 (choice
  (auth "A" (reveal (s1Ab) (choice
                            (revealif (s2Ab a b) (pred (= a b)) (split
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "B")))
                             )
                            (revealif (s2Bb a b) (pred (= a b)) (split
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "B")))
                             )
                            (after 2 (reveal (s2Ad) (split
                                                      (5 -> (withdraw "B")))
                                                    )
                             )
                            (after 2 (reveal (s2Bd) (split
                                                      (5 -> (withdraw "A")))
                                                    )
                             )
                            ))
   )
  (auth "A" (reveal (s1Bb) (choice
                            (revealif (s2Ab a b) (pred (= a b)) (split
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "B")))
                             )
                            (revealif (s2Bb a b) (pred (= a b)) (split
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "A"))
                                                                  (2 -> (withdraw "B")))
                             )
                            (after 2 (reveal (s2Ad) (split
                                                      (5 -> (withdraw "B")))
                                                    )
                             )
                            (after 2 (reveal (s2Bd) (split
                                                      (5 -> (withdraw "A")))
                                                    )
                             )
                            ))
   )
  (after 1 (reveal (s1Ad) (split
                            (5 -> (withdraw "B")))
                          )
   )
  (after 1 (reveal (s1Bd) (split
                            (5 -> (withdraw "A")))
                          )
   )
  (after 101 (revealif (s3Ab a b) (pred (not (pred (= a b)))) (split
                                                                (2 -> (withdraw "B"))
                                                                (2 -> (withdraw "A"))
                                                                (2 -> (withdraw "B")))
              )
   )
  (after 101 (revealif (s3Bb a b) (pred (not (pred (= a b)))) (split
                                                                (2 -> (withdraw "B"))
                                                                (2 -> (withdraw "A"))
                                                                (2 -> (withdraw "B")))
              )
   )
  (after 101 (after 303 (reveal (s3Ad) (split
                                         (5 -> (withdraw "B")))
                                       )
              )
   )
  (after 101 (after 303 (reveal (s3Bd) (split
                                         (5 -> (withdraw "A")))
                                       )
              )
   )
  ))
