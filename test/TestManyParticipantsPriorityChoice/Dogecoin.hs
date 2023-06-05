module TestManyParticipantsPriorityChoice.Dogecoin where

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Coins (DCoins(..))
import Syntax.Common (P(..))
import Syntax.BitML ( (!), D(Withdraw, Split, Reveal, After), G (Secret), C )

participants :: [P]
participants = [pA, pB]

pA = P {pname = "A", pk = "pkA"}
pB = P {pname = "B", pk = "pkB"}
pC = P {pname = "C", pk = "pkC"}


preconditions :: [G DCoins]
preconditions = [
    pA ! 2 $ "dd_A"
    , pB ! 1 $ "dd_B"
    , pC ! 3 $ "dd_C"
    , pA ! 6 $ "dc_A"
    , pB ! 6 $ "dc_B"
    , pC ! 6 $ "dc_C"
    , Secret pA "A_Dogecoin_S_Name_L_" "__SOME_HASH__"
    , Secret pB "B_Dogecoin_S_Name_L_" "__SOME_HASH__"
    , Secret pC "C_Dogecoin_S_Name_L_" "__SOME_HASH__"
    , Secret pA "A_Dogecoin_S_Name_RL_" "__SOME_HASH__"
    , Secret pB "B_Dogecoin_S_Name_RL_" "__SOME_HASH__"
    , Secret pC "C_Dogecoin_S_Name_RL_" "__SOME_HASH__"
    ]

contract :: C DCoins
contract = [
    Reveal ["A_Dogecoin_S_Name_L_"] [
        Split [
            (DCoins 12,[Withdraw pA]),
            (DCoins 6,[Withdraw pB]),
            (DCoins 6,[Withdraw pC])
        ]
    ],
    Reveal ["B_Dogecoin_S_Name_L_"] [
        Split [
            (DCoins 12,[Withdraw pA]),
            (DCoins 6, [Withdraw pB]),
            (DCoins 6, [Withdraw pC])
        ]
    ],
    Reveal ["C_Dogecoin_S_Name_L_"] [
        Split [
            (DCoins 12, [Withdraw pA]),
            (DCoins 6, [Withdraw pB]),
            (DCoins 6,[Withdraw pC])
    ]],
        After 11 (Reveal [] [
            Reveal ["A_Bitcoin_S_Name_L_"] [
                Split [
                    (DCoins 12, [Withdraw pB]),
                    (DCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["B_Bitcoin_S_Name_L_"] [
                Split [
                    (DCoins 12, [Withdraw pA]),
                    (DCoins 12, [Withdraw pC])
                ]
            ],
            Reveal ["C_Bitcoin_S_Name_L_"] [
                Split [
                    (DCoins 12, [Withdraw pA]),
                    (DCoins 12,[Withdraw pB])
                ]],
            After 21 (Reveal [] [
                Reveal ["A_Dogecoin_S_Name_RL_"] [
                    Split [
                        (DCoins 6,[Withdraw pA]),
                        (DCoins 12,[Withdraw pB]),
                        (DCoins 6,[Withdraw pC])
                    ]],
                Reveal ["B_Dogecoin_S_Name_RL_"] [
                    Split [
                        (DCoins 6,[Withdraw pA]),
                        (DCoins 12,[Withdraw pB]),
                        (DCoins 6,[Withdraw pC])
                    ]
                ],
                Reveal ["C_Dogecoin_S_Name_RL_"] [
                    Split [
                        (DCoins 6,[Withdraw pA]),
                        (DCoins 12,[Withdraw pB]),
                        (DCoins 6,[Withdraw pC])
                    ]
                ],
                After 31 (Reveal [] [
                    Reveal ["A_Bitcoin_S_Name_RL_"] [
                        Split [
                            (DCoins 12,[Withdraw pB]),
                            (DCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["B_Bitcoin_S_Name_RL_"] [
                        Split [
                            (DCoins 12,[Withdraw pA]),
                            (DCoins 12,[Withdraw pC])
                        ]
                    ],
                    Reveal ["C_Bitcoin_S_Name_RL_"] [
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
