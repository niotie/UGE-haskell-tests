module ThompsonTests where

import Automaton
import qualified Data.List as L
import qualified Data.Map as M
import Regex
import Test.Tasty
import Test.Tasty.HUnit as HU
import Thompson

test_thompsonBaseCases =
  testGroup
    "Thompson base cases"
    [ testCase "emptyAutomaton" $
        emptyAutomaton
          @?= Automaton
            { initState = 0,
              transTable = M.empty,
              acceptStates = []
            },
      testCase "epsilonAutomaton" $
        epsilonAutomaton
          @?= Automaton
            { initState = 0,
              transTable = M.empty,
              acceptStates = [0]
            },
      testCase "letterAutomaton" $
        letterAutomaton 'a'
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ (Symbol 'a', M.fromList [(0, [1])])
                  ],
              acceptStates = [1]
            }
    ]

test_thompsonTools =
  testGroup
    "Thompson tools tests"
    [ testCase "merge test" $
        transTable exampleAutomaton1 `merge` transTable exampleAutomaton3
          @?= M.fromList
            [ ( Epsilon,
                M.fromList
                  [ (0, [1, 3]),
                    (1, [2]),
                    (3, [4])
                  ]
              ),
              ( Symbol 'a',
                M.fromList
                  [ (0, [1]),
                    (1, [0, 1]),
                    (4, [4])
                  ]
              ),
              ( Symbol 'b',
                M.fromList
                  [ (2, [2]),
                    (3, [3])
                  ]
              )
            ],
      testCase "mapToStates test" $
        mapToStates (+ 100) exampleAutomaton3
          @?= Automaton
            { initState = 100,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (100, [101, 103]),
                          (101, [102]),
                          (103, [104])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (101, [101]),
                          (104, [104])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (102, [102]),
                          (103, [103])
                        ]
                    )
                  ],
              acceptStates = [102, 104]
            }
    ]

test_concat =
  testGroup
    "concat tests"
    [ testCase "concat exampleAutomaton1 exampleAutomaton2" $
        Thompson.concat exampleAutomaton1 exampleAutomaton2
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (0, [2])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (0, [1]),
                          (1, [0]),
                          (2, [2, 3])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (2, [2]),
                          (3, [4])
                        ]
                    ),
                    ( Symbol 'c',
                      M.fromList
                        [ (2, [2]),
                          (4, [5])
                        ]
                    )
                  ],
              acceptStates = [5]
            },
      testCase "concat exampleAutomaton2 exampleAutomaton3" $
        Thompson.concat exampleAutomaton2 exampleAutomaton3
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (3, [4]),
                          (4, [5, 7]),
                          (5, [6]),
                          (7, [8])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (0, [0, 1]),
                          (5, [5]),
                          (8, [8])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (0, [0]),
                          (1, [2]),
                          (6, [6]),
                          (7, [7])
                        ]
                    ),
                    ( Symbol 'c',
                      M.fromList
                        [ (0, [0]),
                          (2, [3])
                        ]
                    )
                  ],
              acceptStates = [6, 8]
            }
    ]

test_union =
  testGroup
    "union tests"
    [ testCase "union exampleAutomaton1 exampleAutomaton2" $
        union exampleAutomaton1 exampleAutomaton2
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (0, [1, 3])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (1, [2]),
                          (2, [1]),
                          (3, [3, 4])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (3, [3]),
                          (4, [5])
                        ]
                    ),
                    ( Symbol 'c',
                      M.fromList
                        [ (3, [3]),
                          (5, [6])
                        ]
                    )
                  ],
              acceptStates = [1, 6]
            },
      testCase "union exampleAutomaton2 exampleAutomaton3" $
        union exampleAutomaton2 exampleAutomaton3
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (0, [1, 5]),
                          (5, [6, 8]),
                          (6, [7]),
                          (8, [9])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (1, [1, 2]),
                          (6, [6]),
                          (9, [9])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (1, [1]),
                          (2, [3]),
                          (7, [7]),
                          (8, [8])
                        ]
                    ),
                    ( Symbol 'c',
                      M.fromList
                        [ (1, [1]),
                          (3, [4])
                        ]
                    )
                  ],
              acceptStates = [4, 7, 9]
            }
    ]

test_star =
  testGroup
    "star tests"
    [ testCase "star exampleAutomaton1" $
        star exampleAutomaton1
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (0, [1]),
                          (1, [0])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (1, [2]),
                          (2, [1])
                        ]
                    )
                  ],
              acceptStates = [0, 1]
            },
      testCase "star exampleAutomaton2" $
        star exampleAutomaton2
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (0, [1]),
                          (4, [0])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (1, [1, 2])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (1, [1]),
                          (2, [3])
                        ]
                    ),
                    ( Symbol 'c',
                      M.fromList
                        [ (1, [1]),
                          (3, [4])
                        ]
                    )
                  ],
              acceptStates = [0, 4]
            },
      testCase "star exampleAutomaton3" $
        star exampleAutomaton3
          @?= Automaton
            { initState = 0,
              transTable =
                M.fromList
                  [ ( Epsilon,
                      M.fromList
                        [ (0, [1]),
                          (1, [2, 4]),
                          (2, [3]),
                          (3, [0]),
                          (4, [5]),
                          (5, [0])
                        ]
                    ),
                    ( Symbol 'a',
                      M.fromList
                        [ (2, [2]),
                          (5, [5])
                        ]
                    ),
                    ( Symbol 'b',
                      M.fromList
                        [ (3, [3]),
                          (4, [4])
                        ]
                    )
                  ],
              acceptStates = [0, 3, 5]
            }
    ]

test_toAutomaton =
  testGroup
    "toAutomaton tests"
    [ testCase "malformed expression" $
        (toRegex "?" >>= pure . toAutomaton)
          @?= Left "Expected '(', '0', '3', or a letter, got ?",
      testCase "aa" $
        (toRegex "aa" >>= pure . toAutomaton)
          @?= Right
            ( Automaton
                { initState = 0,
                  transTable =
                    M.fromList
                      [ ( Epsilon,
                          M.fromList
                            [ (1, [2])
                            ]
                        ),
                        ( Symbol 'a',
                          M.fromList
                            [ (0, [1]),
                              (2, [3])
                            ]
                        )
                      ],
                  acceptStates = [3]
                }
            ),
      testCase "aa|b" $
        (toRegex "aa|b" >>= pure . toAutomaton)
          @?= Right
            ( Automaton
                { initState = 0,
                  transTable =
                    M.fromList
                      [ ( Epsilon,
                          M.fromList
                            [ (0, [1, 5]),
                              (2, [3])
                            ]
                        ),
                        ( Symbol 'a',
                          M.fromList
                            [ (1, [2]),
                              (3, [4])
                            ]
                        ),
                        ( Symbol 'b',
                          M.fromList
                            [ (5, [6])
                            ]
                        )
                      ],
                  acceptStates = [4, 6]
                }
            ),
      testCase "(aa|b)*" $
        (toRegex "(aa|b)*" >>= pure . toAutomaton)
          @?= Right
            ( Automaton
                { initState = 0,
                  transTable =
                    M.fromList
                      [ ( Epsilon,
                          M.fromList
                            [ (0, [1]),
                              (1, [2, 6]),
                              (3, [4]),
                              (5, [0]),
                              (7, [0])
                            ]
                        ),
                        ( Symbol 'a',
                          M.fromList
                            [ (2, [3]),
                              (4, [5])
                            ]
                        ),
                        ( Symbol 'b',
                          M.fromList
                            [ (6, [7])
                            ]
                        )
                      ],
                  acceptStates = [0, 5, 7]
                }
            )
    ]

test_match =
  testGroup
    "match tests"
    [ testCase "is a( in a ?" $
        match "a(" "a"
          @?= Left "Unexpected end of string",
      testCase "is 0 in ε ?" $
        match "0" ""
          @?= Right False,
      testCase "is 0 in a ?" $
        match "0" "a"
          @?= Right False,
      testCase "is 3 in ε ?" $
        match "3" ""
          @?= Right True,
      testCase "is 3 in a ?" $
        match "3" "a"
          @?= Right False,
      testCase "is a in ε ?" $
        match "a" ""
          @?= Right False,
      testCase "is a in a ?" $
        match "a" "a"
          @?= Right True,
      testCase "is (aa)* in aaa ?" $
        match "(aa)*" "aaa"
          @?= Right False,
      testCase "is (aa)* in aaaa ?" $
        match "(aa)*" "aaaa"
          @?= Right True,
      testCase "is (a|b|c)*abc in abccab ?" $
        match "(a|b|c)*abc" "abccab"
          @?= Right False,
      testCase "is (a|b|c)*abc in abccabc ?" $
        match "(a|b|c)*abc" "abccabc"
          @?= Right True,
      testCase "is (b|ab*a)* in bababa ?" $
        match "(b|ab*a)*" "bababa"
          @?= Right False,
      testCase "is (b|ab*a)* in babababba ?" $
        match "(b|ab*a)*" "babababba"
          @?= Right True,
      testCase "is (A|B|...|L)(a|b|...|l)* in babel ?" $
        match "(A|B|C|D|E|F|G|H|I|J|K|L)(a|b|c|d|e|f|g|h|i|j|k|l)*" "babel"
          @?= Right False,
      testCase "is (A|B|...|L)(a|b|...|l)* in Babel ?" $
        match "(A|B|C|D|E|F|G|H|I|J|K|L)(a|b|c|d|e|f|g|h|i|j|k|l)*" "Babel"
          @?= Right True
    ]