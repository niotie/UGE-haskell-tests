module TpfinalTests where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
-- import Test.SmallCheck.Series as SCS

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import Data.Map as M
import Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit
import Tpfinal

tests =
  testGroup
    "Tests for Tpfinal"
    [ ex1Tests,
      ex2Tests,
      ex3Tests,
      ex4Tests
    ]

countPeaksTest =
  testGroup
    "Tests for countPeaks"
    [ testCase "countPeaks []" $
        countPeaks ([] :: [Int]) @?= 0,
      testCase "countPeaks [1]" $
        countPeaks [1] @?= 0,
      testCase "countPeaks [1,2]" $
        countPeaks [1, 2] @?= 0,
      testCase "countPeaks [1,2,3]" $
        countPeaks [1, 2, 3] @?= 0,
      testCase "countPeaks [1,3,2]" $
        countPeaks [1, 3, 2] @?= 1,
      testCase "countPeaks [1,5,2,3,4,5,5]" $
        countPeaks [1, 5, 2, 3, 4, 5, 5] @?= 1,
      testCase "countPeaks [1,5,2,3,4,2,5]" $
        countPeaks [1, 5, 2, 3, 4, 2, 5] @?= 2
    ]

runsTest =
  testGroup
    "Tests for runs"
    [ testCase "runs []" $
        runs ([] :: [Int]) @?= [],
      testCase "runs [1]" $
        runs [1] @?= [[1]],
      testCase "runs [1,2,3,4,5]" $
        runs [1, 2, 3, 4, 5]
          @?= [[1, 2, 3, 4, 5]],
      testCase "runs [1,4,3,5,7,5,4,6,8,3,5]" $
        runs [1, 4, 3, 5, 7, 5, 4, 6, 8, 3, 5]
          @?= [ [1, 4],
                [3, 5, 7],
                [5],
                [4, 6, 8],
                [3, 5]
              ]
    ]

flattenTests =
  testGroup
    "Tests for flatten"
    [ testCase "semiPalindrome \"\"" $
        semiPalindrome "" @?= True,
      testCase "semiPalindrome \"aabb\"" $
        semiPalindrome "aabb" @?= True,
      testCase "semiPalindrome \"aabba\"" $
        semiPalindrome "aabba" @?= True,
      testCase "semiPalindrome \"aabbab\"" $
        semiPalindrome "aabbab" @?= False
    ]

semiPalidromeTests =
  testGroup
    "Tests for semiPalidrome"
    [ testCase "flatten []" $
        flatten ([] :: [[Int]]) @?= [],
      testCase "flatten [[1,2,3]]" $
        flatten [[1, 2, 3]]
          @?= [1, 2, 3],
      testCase "flatten [[1,2,3],[4],[5,6],[7,8,9]]" $
        flatten [[1, 2, 3], [4], [5, 6], [7, 8, 9]]
          @?= [1, 2, 3, 4, 5, 6, 7, 8, 9]
    ]

interleavingsTests =
  testGroup
    "Tests for interleavings"
    [ testCase "interleavings ['a','b','c'] []" $
        interleavings ['a', 'b', 'c'] [] @?= ["abc"],
      testCase "interleavings [] ['A','B']" $
        interleavings [] ['A', 'B'] @?= ["AB"],
      testCase "interleavings ['a','b','c'] ['A','B']" $
        interleavings ['a', 'b', 'c'] ['A', 'B']
          @?= [ "abcAB",
                "abAcB",
                "abABc",
                "aAbcB",
                "aAbBc",
                "aABbc",
                "AabcB",
                "AabBc",
                "AaBbc",
                "ABabc"
              ]
    ]

ex1Tests =
  testGroup
    "Tests for exercise 1"
    [ countPeaksTest,
      runsTest,
      flattenTests,
      semiPalidromeTests,
      interleavingsTests
    ]

ex2Tests =
  testGroup
    "Tests for exercise 2"
    []

addMyMapTests =
  testGroup
    "Tests for addMyMapTests"
    [ testCase "addMyMap 1 myMap1" $
        addMyMap 1 myMap1
          @?= M.fromList
            [ (1, S.fromList [1, 2]),
              (2, S.fromList [1, 3]),
              (3, S.fromList [1, 2, 3, 4])
            ],
      testCase "addMyMap 1 myMap2" $
        addMyMap 1 myMap2
          @?= M.fromList
            [ (2, S.fromList [1, 2]),
              (3, S.fromList [1, 2, 3]),
              (4, S.fromList [1])
            ],
      testCase "addMyMap 1 myMap3" $
        addMyMap 1 myMap3
          @?= M.fromList
            [ (1, S.fromList [1, 2]),
              (3, S.fromList [1, 3]),
              (4, S.fromList [1, 3, 5])
            ]
    ]

onlySingletonMyMapTests =
  testGroup
    "Tests for onlySingletonMyMapTests"
    [ testCase "onlySingletonMyMap myMap1" $
        onlySingletonMyMap myMap1
          @?= M.fromList
            [ (1, S.fromList [2]),
              (2, S.fromList [3])
            ],
      testCase "onlySingletonMyMap myMap2" $
        onlySingletonMyMap myMap2
          @?= M.fromList
            [ (2, S.fromList [2]),
              (4, S.fromList [1])
            ],
      testCase "onlySingletonMyMap myMap3" $
        onlySingletonMyMap myMap3
          @?= M.fromList
            [ (1, S.fromList [2]),
              (3, S.fromList [3])
            ]
    ]

fusionMyMapTests =
  testGroup
    "Tests for fusionMyMapTests"
    [ testCase "fusionMyMap myMap1 myMap2" $
        fusionMyMap myMap1 myMap2
          @?= M.fromList
            [ (1, S.fromList [2]),
              (2, S.fromList [2, 3]),
              (3, S.fromList [1, 2, 3, 4]),
              (4, S.fromList [1])
            ],
      testCase "fusionMyMap myMap1 myMap3" $
        fusionMyMap myMap1 myMap3
          @?= M.fromList
            [ (1, S.fromList [2]),
              (2, S.fromList [3]),
              (3, S.fromList [2, 3, 4]),
              (4, S.fromList [1, 3, 5])
            ],
      testCase "fusionMyMap myMap2 myMap3" $
        fusionMyMap myMap2 myMap3
          @?= M.fromList
            [ (1, S.fromList [2]),
              (2, S.fromList [2]),
              (3, S.fromList [1, 2, 3]),
              (4, S.fromList [1, 3, 5])
            ]
    ]

lookupMyMapTests =
  testGroup
    "Tests for lookupMyMapTests"
    [ testCase "[(k, lookupMyMap k myMap1) | k <- [0 .. 5]]" $
        [(k, lookupMyMap k myMap1) | k <- [0 .. 5]]
          @?= [ (0, Nothing),
                (1, Just [2]),
                (2, Just [3]),
                (3, Just [2, 3]),
                (4, Nothing),
                (5, Nothing)
              ],
      testCase "[(k, lookupMyMap k myMap2) | k <- [0 .. 5]]" $
        [(k, lookupMyMap k myMap2) | k <- [0 .. 5]]
          @?= [ (0, Nothing),
                (1, Nothing),
                (2, Just [2]),
                (3, Just [2, 3]),
                (4, Nothing),
                (5, Nothing)
              ],
      testCase "[(k, lookupMyMap k myMap3) | k <- [0 .. 5]]" $
        [(k, lookupMyMap k myMap3) | k <- [0 .. 5]]
          @?= [ (0, Nothing),
                (1, Nothing),
                (2, Nothing),
                (3, Just [3]),
                (4, Just [1, 3]),
                (5, Nothing)
              ]
    ]

reverseMyMapTests =
  testGroup
    "Tests for reverseMyMapTests"
    [ testCase "reverseMyMap myMap1" $
        reverseMyMap myMap1
          @?= M.fromList
            [ (2, S.fromList [1, 3]),
              (3, S.fromList [2, 3]),
              (4, S.fromList [3])
            ],
      testCase "reverseMyMap myMap2" $
        reverseMyMap myMap2
          @?= M.fromList
            [ (1, S.fromList [3, 4]),
              (2, S.fromList [2, 3]),
              (3, S.fromList [3])
            ],
      testCase "reverseMyMap myMap3" $
        reverseMyMap myMap3
          @?= M.fromList
            [ (1, S.fromList [4]),
              (2, S.fromList [1]),
              (3, S.fromList [3, 4]),
              (5, S.fromList [4])
            ]
    ]

ex3Tests =
  testGroup
    "Tests for exercise 3"
    [ addMyMapTests,
      onlySingletonMyMapTests,
      fusionMyMapTests,
      lookupMyMapTests,
      reverseMyMapTests
    ]

incRLERunTests =
  testGroup
    "Tests for incRLERun"
    [ testCase "incRLERun (mkRLERun 3 'a')" $
        incRLERun (mkRLERun 3 'a')
          @?= (4, 'a'),
      testCase "showRLERun (incRLERun (mkRLERun 3 'a'))" $
        showRLERun (incRLERun (mkRLERun 3 'a'))
          @?= "aaaa"
    ]

decRLERunTests =
  testGroup
    "Tests for decRLERun"
    [ testCase "decRLERun (mkRLERun 3 'a')" $
        decRLERun (mkRLERun 3 'a')
          @?= (2, 'a'),
      testCase "showRLERun (decRLERun (mkRLERun 3 'a'))" $
        showRLERun (decRLERun (mkRLERun 3 'a'))
          @?= "aa"
    ]

consRLEStringTests =
  testGroup
    "Tests for consRLEString"
    [ testCase "consRLEString 'a' emptyRLEString" $
        show (consRLEString 'a' emptyRLEString)
          @?= "\"a\"",
      testCase "showRLEString $ consRLEString 'a' emptyRLEString" $
        showRLEString (consRLEString 'a' emptyRLEString)
          @?= "(1,'a')",
      testCase "consRLEString 'a' $ consRLEString 'a' emptyRLEString" $
        show
          ( consRLEString 'a' $
              consRLEString 'a' emptyRLEString
          )
          @?= "\"aa\"",
      testCase "showRLEString . consRLEString 'a' $ consRLEString 'a' emptyRLEString" $
        showRLEString
          ( consRLEString 'a' $
              consRLEString 'a' emptyRLEString
          )
          @?= "(2,'a')",
      testCase "consRLEString 'b' $ consRLEString 'a' emptyRLEString" $
        show
          ( consRLEString 'b' $
              consRLEString 'a' emptyRLEString
          )
          @?= "\"ba\"",
      testCase "showRLEString . consRLEString 'b' $ consRLEString 'a' emptyRLEString" $
        showRLEString
          ( consRLEString 'b' $
              consRLEString 'a' emptyRLEString
          )
          @?= "(1,'b')(1,'a')"
    ]

fromListRLEStringTests =
  testGroup
    "Tests for fromListRLEString"
    [ testCase "showRLEString $ fromListRLEString []" $
        showRLEString (fromListRLEString [])
          @?= "",
      testCase "showRLEString $ fromListRLEString \"a\"" $
        showRLEString (fromListRLEString "a")
          @?= "(1,'a')",
      testCase "showRLEString $ fromListRLEString \"aabbbabcc\"" $
        showRLEString (fromListRLEString "aabbbabcc")
          @?= "(2,'a')(3,'b')(1,'a')(1,'b')(2,'c')"
    ]

headRLEStringTests =
  testGroup
    "Tests for headRLEString"
    [ testCase "headRLEString $ fromListRLEString []" $
        headRLEString (fromListRLEString [])
          @?= Nothing,
      testCase "headRLEString $ fromListRLEString \"a\"" $
        headRLEString (fromListRLEString "a")
          @?= Just 'a',
      testCase "headRLEString $ fromListRLEString \"aabbbabcc\"" $
        headRLEString (fromListRLEString "aabbbabcc")
          @?= Just 'a'
    ]

tailRLEStringTests =
  testGroup
    "Tests for tailRLEString"
    [ testCase "tailRLEString $ fromListRLEString []" $
        tailRLEString (fromListRLEString [])
          @?= Nothing,
      testCase "tailRLEString $ fromListRLEString \"a\"" $
        show <$> tailRLEString (fromListRLEString "a")
          @?= Just "\"\"",
      testCase "tailRLEString $ fromListRLEString \"aabbbabcc\"" $
        show <$> tailRLEString (fromListRLEString "aabbbabcc")
          @?= Just "\"abbbabcc\"",
      testCase "tailRLEString $ fromListRLEString \"abbbabcc\"" $
        show <$> tailRLEString (fromListRLEString "abbbabcc")
          @?= Just "\"bbbabcc\""
    ]

initRLEStringTests =
  testGroup
    "Tests for initRLEString"
    [ testCase "initRLEString $ fromListRLEString []" $
        initRLEString (fromListRLEString [])
          @?= Nothing,
      testCase "initRLEString $ fromListRLEString \"a\"" $
        show <$> initRLEString (fromListRLEString "a")
          @?= Just "\"\"",
      testCase "initRLEString $ fromListRLEString \"aabbbabcc\"" $
        show <$> initRLEString (fromListRLEString "aabbbabcc")
          @?= Just "\"aabbbabc\"",
      testCase "initRLEString $ fromListRLEString \"aabbbabc\"" $
        show <$> initRLEString (fromListRLEString "aabbbabc")
          @?= Just "\"aabbbab\""
    ]

concatRLEStringTests =
  testGroup
    "Tests for (@++@)"
    [ testCase "showRLEString $ fromListRLEString \"aabb\" @++@ fromListRLEString \"ccdd\"" $
        showRLEString
          ( fromListRLEString "aabb"
              @++@ fromListRLEString "ccdd"
          )
          @?= "(2,'a')(2,'b')(2,'c')(2,'d')",
      testCase "showRLEString $ fromListRLEString \"aabb\" @++@ fromListRLEString \"bbdd\"" $
        showRLEString
          ( fromListRLEString "aabb"
              @++@ fromListRLEString "bbdd"
          )
          @?= "(2,'a')(4,'b')(2,'d')"
    ]

mapRLEStringTests =
  testGroup
    "Tests for mapRLEString"
    [ testCase "toUpper" $
        showRLEString (mapRLEString toUpper $ fromListRLEString "abbccc")
          @?= "(1,'A')(2,'B')(3,'C')",
      testCase "const 'A'" $
        showRLEString (mapRLEString (const 'A') $ fromListRLEString "abbcccdddd")
          @?= "(10,'A')",
      testCase "\\x -> if x `L.elem` ['A'..'L'] then '0' else '1'" $
        showRLEString
          ( mapRLEString
              (\x -> if x `elem` ['A' .. 'L'] then '0' else '1')
              $ fromListRLEString ['A' .. 'Z']
          )
          @?= "(12,'0')(14,'1')"
    ]

palindromeRLEStringTests =
  testGroup
    "Tests for palindromeRLEString"
    [ testCase "palindromeRLEString $ fromListRLEString \"abbcccbba\"" $
        palindromeRLEString (fromListRLEString "abbcccbba")
          @?= True,
      testCase "palindromeRLEString $ fromListRLEString \"abbbaa\"" $
        palindromeRLEString (fromListRLEString "abbbaa")
          @?= False
    ]

takeRLEStringTests =
  testGroup
    "Tests for takeRLEString"
    [ testCase "[(i, takeRLEString i (fromListRLEString \"abbcccdddd\")) | i <- [-1..11]]" $
        [ (i, show (takeRLEString i (fromListRLEString "abbcccdddd")))
          | i <- [-1 .. 11]
        ]
          @?= [ (-1, "\"\""),
                (0, "\"\""),
                (1, "\"a\""),
                (2, "\"ab\""),
                (3, "\"abb\""),
                (4, "\"abbc\""),
                (5, "\"abbcc\""),
                (6, "\"abbccc\""),
                (7, "\"abbcccd\""),
                (8, "\"abbcccdd\""),
                (9, "\"abbcccddd\""),
                (10, "\"abbcccdddd\""),
                (11, "\"abbcccdddd\"")
              ]
    ]

dropRLEStringTests =
  testGroup
    "Tests for dropRLEString"
    [ testCase "[(i, dropRLEString i (fromListRLEString \"abbcccdddd\")) | i <- [-1..11]]" $
        [ (i, show $ dropRLEString i (fromListRLEString "abbcccdddd"))
          | i <- [-1 .. 11]
        ]
          @?= [ (-1, "\"abbcccdddd\""),
                (0, "\"abbcccdddd\""),
                (1, "\"bbcccdddd\""),
                (2, "\"bcccdddd\""),
                (3, "\"cccdddd\""),
                (4, "\"ccdddd\""),
                (5, "\"cdddd\""),
                (6, "\"dddd\""),
                (7, "\"ddd\""),
                (8, "\"dd\""),
                (9, "\"d\""),
                (10, "\"\""),
                (11, "\"\"")
              ]
    ]

splitAtRLEStringTests =
  testGroup
    "Tests for splitAtRLEString"
    [ testCase "[(i, splitAtRLEString i (fromListRLEString \"abbcccdddd\")) | i <- [-1..11]]" $
        [(i, join bimap show $ splitAtRLEString i (fromListRLEString "abbcccdddd")) | i <- [-1 .. 11]]
          @?= [ (-1, ("\"\"", "\"abbcccdddd\"")),
                (0, ("\"\"", "\"abbcccdddd\"")),
                (1, ("\"a\"", "\"bbcccdddd\"")),
                (2, ("\"ab\"", "\"bcccdddd\"")),
                (3, ("\"abb\"", "\"cccdddd\"")),
                (4, ("\"abbc\"", "\"ccdddd\"")),
                (5, ("\"abbcc\"", "\"cdddd\"")),
                (6, ("\"abbccc\"", "\"dddd\"")),
                (7, ("\"abbcccd\"", "\"ddd\"")),
                (8, ("\"abbcccdd\"", "\"dd\"")),
                (9, ("\"abbcccddd\"", "\"d\"")),
                (10, ("\"abbcccdddd\"", "\"\"")),
                (11, ("\"abbcccdddd\"", "\"\""))
              ]
    ]

ex4Tests =
  testGroup
    "Tests for exercise 4"
    [ incRLERunTests,
      decRLERunTests,
      consRLEStringTests,
      fromListRLEStringTests,
      headRLEStringTests,
      tailRLEStringTests,
      initRLEStringTests,
      concatRLEStringTests,
      mapRLEStringTests,
      palindromeRLEStringTests,
      takeRLEStringTests,
      dropRLEStringTests,
      splitAtRLEStringTests
    ]