module AutomatonTests where

import Automaton
import qualified Data.List as L
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit as HU

test_parseAtom =
  testGroup
    "mkTransTable tests"
    [ testCase "automaton 1 transition table" $
        testMe
          [ (Symbol 'a', 0, 1),
            (Symbol 'a', 1, 0)
          ]
          @?= [(Symbol 'a', [(0, [1]), (1, [0])])],
      testCase "automaton 2 transition table" $
        testMe
          [ (Symbol 'a', 0, 0),
            (Symbol 'a', 0, 1),
            (Symbol 'b', 0, 0),
            (Symbol 'b', 1, 2),
            (Symbol 'c', 0, 0),
            (Symbol 'c', 2, 3)
          ]
          @?= [ (Symbol 'a', [(0, [0, 1])]),
                (Symbol 'b', [(0, [0]), (1, [2])]),
                (Symbol 'c', [(0, [0]), (2, [3])])
              ],
      testCase "automaton 3 transition table" $
        testMe
          [ (Epsilon, 0, 1),
            (Epsilon, 0, 3),
            (Epsilon, 1, 2),
            (Epsilon, 3, 4),
            (Symbol 'a', 1, 1),
            (Symbol 'a', 4, 4),
            (Symbol 'b', 2, 2),
            (Symbol 'b', 3, 3)
          ]
          @?= [ (Epsilon, [(0, [1, 3]), (1, [2]), (3, [4])]),
                (Symbol 'a', [(1, [1]), (4, [4])]),
                (Symbol 'b', [(2, [2]), (3, [3])])
              ]
    ]
  where
    testMe = M.toList . M.map (L.sort . M.toList) . mkTransTable

test_alphabet =
  testGroup
    "alphabet tests"
    [ testCase "automaton 1" $ alphabet exampleAutomaton1 @?= "a",
      testCase "automaton 2" $ alphabet exampleAutomaton2 @?= "abc",
      testCase "automaton 3" $ alphabet exampleAutomaton3 @?= "ab"
    ]

test_states =
  testGroup
    "states tests"
    [ testCase "automaton 1" $ L.sort (states exampleAutomaton1) @?= [0, 1],
      testCase "automaton 2" $ L.sort (states exampleAutomaton2) @?= [0, 1, 2, 3],
      testCase "automaton 3" $ L.sort (states exampleAutomaton3) @?= [0, 1, 2, 3, 4],
      testCase "automaton 3 with extra init state" $
        L.sort (states exampleAutomaton3 {initState = 7}) @?= [0, 1, 2, 3, 4, 7],
      testCase "automaton 3 with extra accepting states" $
        L.sort (states exampleAutomaton3 {acceptStates = [8, 9]}) @?= [0, 1, 2, 3, 4, 8, 9]
    ]

test_maxState =
  testGroup
    "maxState tests"
    [ testCase "automaton 1" $ maxState exampleAutomaton1 @?= 1,
      testCase "automaton 3" $ maxState exampleAutomaton3 @?= 4,
      testCase "automaton 3 with extra init state" $
        maxState exampleAutomaton3 {initState = 7} @?= 7
    ]

test_transFunc =
  testGroup
    "transFunc tests"
    [ testCase "a from 0 in automaton 1" $
        transFunc exampleAutomaton1 (Symbol 'a') 0
          @?= [1],
      testCase "a from 0 in automaton 2" $
        transFunc exampleAutomaton2 (Symbol 'a') 0
          @?= [0, 1],
      testCase "a from 0 in automaton 3" $
        transFunc exampleAutomaton3 (Symbol 'a') 0
          @?= [],
      testCase "x from 0 in automaton 3" $
        transFunc exampleAutomaton3 (Symbol 'x') 0
          @?= [],
      testCase "ε from 0 in automaton 3" $
        transFunc exampleAutomaton3 Epsilon 0
          @?= [1, 3]
    ]

test_untilFixpoint =
  testGroup
    "untilFixpoint tests"
    [ testCase "(div 1) from 8" $
        untilFixpoint (`div` 1) 8
          @?= 8,
      testCase "(div 2) from 8" $
        untilFixpoint (`div` 2) 8
          @?= 0,
      testCase "(* 1) from 8" $
        untilFixpoint (* 1) 8
          @?= 8,
      testCase "(/ 2) from 1" $
        untilFixpoint (/ 2) 1
          @?= 0
    ]

test_epsilonClosure =
  testGroup
    "epsilonClosure tests"
    [ testCase "from 0 in automaton 1" $
        L.sort (epsilonClosure exampleAutomaton1 0)
          @?= [0],
      testCase "from 0 in automaton 2" $
        L.sort (epsilonClosure exampleAutomaton2 0)
          @?= [0],
      testCase "from 0 in automaton 3" $
        L.sort (epsilonClosure exampleAutomaton3 0)
          @?= [0, 1, 2, 3, 4],
      testCase "from 1 in automaton 3" $
        L.sort (epsilonClosure exampleAutomaton3 1)
          @?= [1, 2],
      testCase "from 2 in automaton 3" $
        L.sort (epsilonClosure exampleAutomaton3 2)
          @?= [2]
    ]

test_reachedStates =
  testGroup
    "reachedStates tests"
    [ testCase "on aa in automaton 1" $
        reachedStates exampleAutomaton1 "aa" @?= [0],
      testCase "on aaa in automaton 1" $
        reachedStates exampleAutomaton1 "aaa" @?= [1],
      testCase "on abc in automaton 2" $
        reachedStates exampleAutomaton2 "abc" @?= [0, 3],
      testCase "on abca in automaton 2" $
        reachedStates exampleAutomaton2 "abca" @?= [0, 1],
      testCase "on ε in automaton 3" $
        reachedStates exampleAutomaton3 "" @?= [0, 1, 3, 2, 4],
      testCase "on a in automaton 3" $
        reachedStates exampleAutomaton3 "a" @?= [1, 2, 4],
      testCase "on ab in automaton 3" $
        reachedStates exampleAutomaton3 "ab" @?= [2],
      testCase "on aba in automaton 3" $
        reachedStates exampleAutomaton3 "aba" @?= [],
      testCase "on abc in automaton 3" $
        reachedStates exampleAutomaton3 "abc" @?= []
    ]

test_accepts =
  testGroup
    "accepts tests"
    [ testCase "on aa in automaton 1" $
        accepts exampleAutomaton1 "aa" @?= True,
      testCase "on aaa in automaton 1" $
        accepts exampleAutomaton1 "aaa" @?= False,
      testCase "on abc in automaton 2" $
        accepts exampleAutomaton2 "abc" @?= True,
      testCase "on abca in automaton 2" $
        accepts exampleAutomaton2 "abca" @?= False,
      testCase "on ε in automaton 3" $
        accepts exampleAutomaton3 "" @?= True,
      testCase "on a in automaton 3" $
        accepts exampleAutomaton3 "a" @?= True,
      testCase "on ab in automaton 3" $
        accepts exampleAutomaton3 "ab" @?= True,
      testCase "on aba in automaton 3" $
        accepts exampleAutomaton3 "aba" @?= False,
      testCase "on abc in automaton 3" $
        accepts exampleAutomaton3 "abc" @?= False
    ]
