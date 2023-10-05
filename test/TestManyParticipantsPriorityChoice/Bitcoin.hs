module TestManyParticipantsPriorityChoice.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), GuardedContract(Withdraw, Split, Reveal, After), Precondition (Secret), Contract )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [Precondition BCoins]
preconditions = [
    pA ! 7 $ "A_deposit_Bitcoin"
    , pB ! 8 $ "B_deposit_Bitcoin"
    , pC ! 9 $ "C_deposit_Bitcoin"
    , Secret pA "StepSecret_A__L_" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B__L_" "__HASH__PLACEHOLDER__"
    , Secret pC "StepSecret_C__L_" "__HASH__PLACEHOLDER__"
    , Secret pA "StepSecret_A__RL_" "__HASH__PLACEHOLDER__"
    , Secret pB "StepSecret_B__RL_" "__HASH__PLACEHOLDER__"
    , Secret pC "StepSecret_C__RL_" "__HASH__PLACEHOLDER__"
    , Secret pA "StartSecret_A" "__HASH__PLACEHOLDER__"
    , Secret pB "StartSecret_B" "__HASH__PLACEHOLDER__"
    , Secret pC "StartSecret_C" "__HASH__PLACEHOLDER__"
    ]

contract :: Contract BCoins
contract = [
    Reveal ["StepSecret_A__L_"] [
        Split [
            (BCoins 12,[Withdraw pA]),
            (BCoins 6,[Withdraw pB]),
            (BCoins 6,[Withdraw pC])
        ]
    ],
    Reveal ["StepSecret_B__L_"] [
        Split [
            (BCoins 12,[Withdraw pA]),
            (BCoins 6, [Withdraw pB]),
            (BCoins 6, [Withdraw pC])
        ]
    ],
    Reveal ["StepSecret_C__L_"] [
        Split [
            (BCoins 12, [Withdraw pA]),
            (BCoins 6, [Withdraw pB]),
            (BCoins 6,[Withdraw pC])
    ]],
        After 11 (Reveal [] [
            Reveal ["StepSecret_A__L_"] [
                Split [
                    (BCoins 12, [Withdraw pB]),
                    (BCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["StepSecret_B__L_"] [
                Split [
                    (BCoins 12, [Withdraw pA]),
                    (BCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["StepSecret_C__L_"] [
                Split [
                    (BCoins 12, [Withdraw pA]),
                    (BCoins 12,[Withdraw pB])
                ]],
            After 21 (Reveal [] [
                Reveal ["StepSecret_A__RL_"] [
                    Split [
                        (BCoins 6, [Withdraw pA]),
                        (BCoins 12,[Withdraw pB]),
                        (BCoins 6,[Withdraw pC])
                    ]
                ],
                Reveal ["StepSecret_B__RL_"] [
                    Split [
                        (BCoins 6, [Withdraw pA]),
                        (BCoins 12,[Withdraw pB]),
                        (BCoins 6,[Withdraw pC])
                    ]
                ],
                Reveal ["StepSecret_C__RL_"] [
                    Split [
                        (BCoins 6,[Withdraw pA]),
                        (BCoins 12,[Withdraw pB]),
                        (BCoins 6,[Withdraw pC])
                    ]
                ],
                After 31 (Reveal [] [
                    Reveal ["StepSecret_A__RL_"] [
                        Split [
                            (BCoins 12,[Withdraw pB]),
                            (BCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["StepSecret_B__RL_"] [
                        Split [
                            (BCoins 12,[Withdraw pA]),
                            (BCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["StepSecret_C__RL_"] [
                        Split [
                            (BCoins 12,[Withdraw pA]),
                            (BCoins 12,[Withdraw pB])
                        ]
                    ],
                    After 41 (Reveal [] [
                        Split [
                            (BCoins 6,[Withdraw pA]),
                            (BCoins 12,[Withdraw pB]),
                            (BCoins 6,[Withdraw pC])
                        ]
                    ])
                ])
            ])
        ])
    ]

