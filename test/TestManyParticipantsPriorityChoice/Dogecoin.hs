module TestManyParticipantsPriorityChoice.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), GuardedContract(Withdraw, Split, Reveal, After), Precondition (Secret), Contract )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [Precondition DCoins]
preconditions = [
    pA ! 8 $ "A_deposit_Dogecoin"
    , pB ! 7 $ "B_deposit_Dogecoin"
    , pC ! 9 $ "C_deposit_Dogecoin"
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

contract :: Contract DCoins
contract = [
    Reveal ["StepSecret_A__L_"] [
        Split [
            (DCoins 12,[Withdraw pA]),
            (DCoins 6,[Withdraw pB]),
            (DCoins 6,[Withdraw pC])
        ]
    ],
    Reveal ["StepSecret_B__L_"] [
        Split [
            (DCoins 12,[Withdraw pA]),
            (DCoins 6, [Withdraw pB]),
            (DCoins 6, [Withdraw pC])
        ]
    ],
    Reveal ["StepSecret_C__L_"] [
        Split [
            (DCoins 12, [Withdraw pA]),
            (DCoins 6, [Withdraw pB]),
            (DCoins 6,[Withdraw pC])
    ]],
        After 11 (Reveal [] [
            Reveal ["StepSecret_A__L_"] [
                Split [
                    (DCoins 12, [Withdraw pB]),
                    (DCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["StepSecret_B__L_"] [
                Split [
                    (DCoins 12, [Withdraw pA]),
                    (DCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["StepSecret_C__L_"] [
                Split [
                    (DCoins 12, [Withdraw pA]),
                    (DCoins 12,[Withdraw pB])
                ]],
            After 21 (Reveal [] [
                Reveal ["StepSecret_A__RL_"] [
                    Split [
                        (DCoins 6,[Withdraw pA]),
                        (DCoins 12,[Withdraw pB]),
                        (DCoins 6,[Withdraw pC])
                    ]],
                Reveal ["StepSecret_B__RL_"] [
                    Split [
                        (DCoins 6,[Withdraw pA]),
                        (DCoins 12,[Withdraw pB]),
                        (DCoins 6,[Withdraw pC])
                    ]
                ],
                Reveal ["StepSecret_C__RL_"] [
                    Split [
                        (DCoins 6,[Withdraw pA]),
                        (DCoins 12,[Withdraw pB]),
                        (DCoins 6,[Withdraw pC])
                    ]
                ],
                After 31 (Reveal [] [
                    Reveal ["StepSecret_A__RL_"] [
                        Split [
                            (DCoins 12,[Withdraw pB]),
                            (DCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["StepSecret_B__RL_"] [
                        Split [
                            (DCoins 12,[Withdraw pA]),
                            (DCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["StepSecret_C__RL_"] [
                        Split [
                            (DCoins 12,[Withdraw pA]),
                            (DCoins 12,[Withdraw pB])
                        ]
                    ],
                    After 41 (Reveal [] [
                        Split [
                            (DCoins 6,[Withdraw pA]),
                            (DCoins 12,[Withdraw pB]),
                            (DCoins 6,[Withdraw pC])
                        ]
                    ])
                ])
            ])
        ])
    ]
