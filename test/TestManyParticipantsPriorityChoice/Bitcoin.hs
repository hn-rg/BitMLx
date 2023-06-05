module TestManyParticipantsPriorityChoice.Bitcoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (BCoins(..), )
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret), C )


participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [G BCoins]
preconditions = [
    pA ! 1 $ "bd_A"
    , pB ! 2 $ "bd_B"
    , pC ! 3 $ "bd_C"
    , pA ! 6 $ "bc_A"
    , pB ! 6 $ "bc_B"
    , pC ! 6 $ "bc_C"
    , Secret pA "A_Bitcoin_S_Name_L_" "__SOME_HASH__"
    , Secret pB "B_Bitcoin_S_Name_L_" "__SOME_HASH__"
    , Secret pC "C_Bitcoin_S_Name_L_" "__SOME_HASH__"
    , Secret pA "A_Bitcoin_S_Name_RL_" "__SOME_HASH__"
    , Secret pB "B_Bitcoin_S_Name_RL_" "__SOME_HASH__"
    , Secret pC "C_Bitcoin_S_Name_RL_" "__SOME_HASH__"
    ]

contract :: C BCoins
contract = [
    Reveal ["A_Bitcoin_S_Name_L_"] [
        Split [
            (BCoins 12,[Withdraw pA]),
            (BCoins 6,[Withdraw pB]),
            (BCoins 6,[Withdraw pC])
        ]
    ],
    Reveal ["B_Bitcoin_S_Name_L_"] [
        Split [
            (BCoins 12,[Withdraw pA]),
            (BCoins 6, [Withdraw pB]),
            (BCoins 6, [Withdraw pC])
        ]
    ],
    Reveal ["C_Bitcoin_S_Name_L_"] [
        Split [
            (BCoins 12, [Withdraw pA]),
            (BCoins 6, [Withdraw pB]),
            (BCoins 6,[Withdraw pC])
    ]],
        After 11 (Reveal [] [
            Reveal ["A_Dogecoin_S_Name_L_"] [
                Split [
                    (BCoins 12, [Withdraw pB]),
                    (BCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["B_Dogecoin_S_Name_L_"] [
                Split [
                    (BCoins 12, [Withdraw pA]),
                    (BCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["C_Dogecoin_S_Name_L_"] [
                Split [
                    (BCoins 12, [Withdraw pA]),
                    (BCoins 12,[Withdraw pB])
                ]],
            After 21 (Reveal [] [
                Reveal ["A_Bitcoin_S_Name_RL_"] [
                    Split [
                        (BCoins 6, [Withdraw pA]),
                        (BCoins 12,[Withdraw pB]),
                        (BCoins 6,[Withdraw pC])
                    ]
                ],
                Reveal ["B_Bitcoin_S_Name_RL_"] [
                    Split [
                        (BCoins 6, [Withdraw pA]),
                        (BCoins 12,[Withdraw pB]),
                        (BCoins 6,[Withdraw pC])
                    ]
                ],
                Reveal ["C_Bitcoin_S_Name_RL_"] [
                    Split [
                        (BCoins 6,[Withdraw pA]),
                        (BCoins 12,[Withdraw pB]),
                        (BCoins 6,[Withdraw pC])
                    ]
                ],
                After 31 (Reveal [] [
                    Reveal ["A_Dogecoin_S_Name_RL_"] [
                        Split [
                            (BCoins 12,[Withdraw pB]),
                            (BCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["B_Dogecoin_S_Name_RL_"] [
                        Split [
                            (BCoins 12,[Withdraw pA]),
                            (BCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["C_Dogecoin_S_Name_RL_"] [
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

