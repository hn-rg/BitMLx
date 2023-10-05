#lang bitml

(debug-mode)

(participant "A" "pkA")
(participant "B" "pkB")

(contract
 (pre
  (deposit "A" 1 "A_deposit_Dogecoin")
  (deposit "B" 1 "B_deposit_Dogecoin")
  (secret "A" a "__SOME_HASH__")
  (secret "B" b "__SOME_HASH__")
  (secret "A" StepSecret_A__L_ "__HASH__PLACEHOLDER__")
  (secret "B" StepSecret_B__L_ "__HASH__PLACEHOLDER__")
  (secret "A" StepSecret_A__LL_ "__HASH__PLACEHOLDER__")
  (secret "B" StepSecret_B__LL_ "__HASH__PLACEHOLDER__")
  (secret "A" StepSecret_A__LRL_ "__HASH__PLACEHOLDER__")
  (secret "B" StepSecret_B__LRL_ "__HASH__PLACEHOLDER__")
  (secret "A" StepSecret_A__LRRL_ "__HASH__PLACEHOLDER__")
  (secret "B" StepSecret_B__LRRL_ "__HASH__PLACEHOLDER__")
  (secret "A" StartSecret_A "__HASH__PLACEHOLDER__")
  (secret "B" StartSecret_B "__HASH__PLACEHOLDER__")
  )

 (choice
  (reveal (StepSecret_A__L_ StartSecret_A StartSecret_B) (choice
                                                          (revealif (StepSecret_A__LL_ a b) (pred (and (between b 0 1) (= a b))) (withdraw "A"))
                                                          (revealif (StepSecret_B__LL_ a b) (pred (and (between b 0 1) (= a b))) (withdraw "A"))
                                                          (after 21 (reveal () (choice
                                                                                (reveal (StepSecret_A__LL_) (withdraw "B"))
                                                                                (reveal (StepSecret_B__LL_) (withdraw "A"))
                                                                                (after 31 (reveal () (choice
                                                                                                      (revealif (StepSecret_A__LRL_ a b) (pred (and (between b 0 1) (!= a b))) (withdraw "B"))
                                                                                                      (revealif (StepSecret_B__LRL_ a b) (pred (and (between b 0 1) (!= a b))) (withdraw "B"))
                                                                                                      (after 41 (reveal () (choice
                                                                                                                            (reveal (StepSecret_A__LRL_) (withdraw "B"))
                                                                                                                            (reveal (StepSecret_B__LRL_) (withdraw "A"))
                                                                                                                            (after 51 (reveal () (choice
                                                                                                                                                  (revealif (StepSecret_A__LRRL_ a b) (pred (between b 0 1)) (withdraw "B"))
                                                                                                                                                  (revealif (StepSecret_B__LRRL_ a b) (pred (between b 0 1)) (withdraw "B"))
                                                                                                                                                  (after 61 (reveal () (choice
                                                                                                                                                                        (reveal (StepSecret_A__LRRL_) (withdraw "B"))
                                                                                                                                                                        (reveal (StepSecret_B__LRRL_) (withdraw "A"))
                                                                                                                                                                        (after 71 (reveal () (withdraw "A"))
                                                                                                                                                                         )
                                                                                                                                                                        ))
                                                                                                                                                   )
                                                                                                                                                  ))
                                                                                                                             )
                                                                                                                            ))
                                                                                                       )
                                                                                                      ))
                                                                                 )
                                                                                ))
                                                           )
                                                          ))
  (reveal (StepSecret_B__L_ StartSecret_A StartSecret_B) (choice
                                                          (revealif (StepSecret_A__LL_ a b) (pred (and (between b 0 1) (= a b))) (withdraw "A"))
                                                          (revealif (StepSecret_B__LL_ a b) (pred (and (between b 0 1) (= a b))) (withdraw "A"))
                                                          (after 21 (reveal () (choice
                                                                                (reveal (StepSecret_A__LL_) (withdraw "B"))
                                                                                (reveal (StepSecret_B__LL_) (withdraw "A"))
                                                                                (after 31 (reveal () (choice
                                                                                                      (revealif (StepSecret_A__LRL_ a b) (pred (and (between b 0 1) (!= a b))) (withdraw "B"))
                                                                                                      (revealif (StepSecret_B__LRL_ a b) (pred (and (between b 0 1) (!= a b))) (withdraw "B"))
                                                                                                      (after 41 (reveal () (choice
                                                                                                                            (reveal (StepSecret_A__LRL_) (withdraw "B"))
                                                                                                                            (reveal (StepSecret_B__LRL_) (withdraw "A"))
                                                                                                                            (after 51 (reveal () (choice
                                                                                                                                                  (revealif (StepSecret_A__LRRL_ a b) (pred (between b 0 1)) (withdraw "B"))
                                                                                                                                                  (revealif (StepSecret_B__LRRL_ a b) (pred (between b 0 1)) (withdraw "B"))
                                                                                                                                                  (after 61 (reveal () (choice
                                                                                                                                                                        (reveal (StepSecret_A__LRRL_) (withdraw "B"))
                                                                                                                                                                        (reveal (StepSecret_B__LRRL_) (withdraw "A"))
                                                                                                                                                                        (after 71 (reveal () (withdraw "A"))
                                                                                                                                                                         )
                                                                                                                                                                        ))
                                                                                                                                                   )
                                                                                                                                                  ))
                                                                                                                             )
                                                                                                                            ))
                                                                                                       )
                                                                                                      ))
                                                                                 )
                                                                                ))
                                                           )
                                                          ))
  (after 11 (reveal () (choice
                        (reveal (StepSecret_A__L_) (withdraw "B"))
                        (reveal (StepSecret_B__L_) (withdraw "A"))
                        (after 21 (reveal () (split
                                               (1 -> (withdraw "A"))
                                               (1 -> (withdraw "B")))
                                             )
                         )
                        ))
   )
  ))
