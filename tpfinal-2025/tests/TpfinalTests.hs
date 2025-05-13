module TpfinalTests where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
-- import Test.SmallCheck.Series as SCS

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Char (toUpper)
import Data.List as L
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
      ex3Tests
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
    [ testCase "flatten []" $
        flatten ([] :: [[Int]]) @?= [],
      testCase "flatten [[1,2,3]]" $
        flatten [[1, 2, 3]]
          @?= [1, 2, 3],
      testCase "flatten [[1,2,3],[4],[5,6],[7,8,9]]" $
        flatten [[1, 2, 3], [4], [5, 6], [7, 8, 9]]
          @?= [1, 2, 3, 4, 5, 6, 7, 8, 9]
    ]

semiPalindromeTests =
  testGroup
    "Tests for semiPalindrome"
    [ testCase "semiPalindrome \"\"" $
        semiPalindrome "" @?= True,
      testCase "semiPalindrome \"aabb\"" $
        semiPalindrome "aabb" @?= True,
      testCase "semiPalindrome \"aabba\"" $
        semiPalindrome "aabba" @?= True,
      testCase "semiPalindrome \"aabbab\"" $
        semiPalindrome "aabbab" @?= False,
      testCase "semiPalindrome . L.concat $ L.replicate 100 \"abc\"" $
        ( semiPalindrome . L.concat $
            L.replicate 100 "abc"
        )
          @?= True,
      testCase "semiPalindrome . (++) \"a\" . L.concat $ L.replicate 100 \"abc\"" $
        ( semiPalindrome . (++) "a" . L.concat $
            L.replicate 100 "abc"
        )
          @?= True,
      testCase "semiPalindrome . (++) \"ab\" . L.concat $ L.replicate 100 \"abc\"" $
        ( semiPalindrome . (++) "ab" . L.concat $
            L.replicate 100 "abc"
        )
          @?= False
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

squareInterleavingTests =
  testGroup
    "Tests for squareInterleaving"
    [ testCase "squareInterleaving \"\"" $
        squareInterleaving "" @?= True,
      -- testCase "\"\" `L.elem` interleavings \"\" \"\"" $
      --   ("" `L.elem` interleavings "" "") @?= True,
      testCase "squareInterleaving \"aabb\"" $
        squareInterleaving "aabb" @?= True,
      -- testCase "\"aabb\" `L.elem` interleavings \"ab\" \"ab\"" $
      --   "aabb" `L.elem` interleavings "ab" "ab" @?= True,
      testCase "squareInterleaving \"aabbaababa\"" $
        squareInterleaving "aabbaababa" @?= True,
      -- testCase "\"aabbaababa\" `L.elem` interleavings \"aabba\" \"aabba\"" $
      --   "aabbaababa" `L.elem` interleavings "aabba" "aabba" @?= True,
      testCase "squareInterleaving \"aabbaababa\"" $
        squareInterleaving "abbaaababa" @?= False
    ]

ex1Tests =
  testGroup
    "Tests for exercise 1"
    [ countPeaksTest,
      runsTest,
      flattenTests,
      semiPalindromeTests,
      interleavingsTests,
      squareInterleavingTests
    ]

singlesMyMapTests =
  testGroup
    "Tests for singlesMyMapTests"
    [ testCase "singlesMyMap []" $
        singlesMyMap ([] :: [Int])
          @?= M.empty,
      testCase "singlesMyMap [1..3]" $
        singlesMyMap [1 .. 3]
          @?= M.fromList
            [ (1, S.fromList [1]),
              (2, S.fromList [2]),
              (3, S.fromList [3])
            ],
      testCase "singlesMyMap $ L.concat [L.replicate 3 x | x <- [1..3]]" $
        singlesMyMap
          (L.concat [L.replicate 3 x | x <- [1 .. 3]])
          @?= M.fromList
            [ (1, S.fromList [1]),
              (2, S.fromList [2]),
              (3, S.fromList [3])
            ]
    ]

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

ex2Tests =
  testGroup
    "Tests for exercise 2"
    [ addMyMapTests,
      onlySingletonMyMapTests,
      fusionMyMapTests,
      lookupMyMapTests,
      reverseMyMapTests
    ]

incRLERTests =
  testGroup
    "Tests for incRLER"
    [ testCase "incRLER (mkRLER 3 'a')" $
        incRLER (mkRLER 3 'a')
          @?= (4, 'a'),
      testCase "showRLER (incRLER (mkRLER 3 'a'))" $
        showRLER (incRLER (mkRLER 3 'a'))
          @?= "aaaa"
    ]

decRLERTests =
  testGroup
    "Tests for decRLER"
    [ testCase "decRLER (mkRLER 3 'a')" $
        decRLER (mkRLER 3 'a')
          @?= (2, 'a'),
      testCase "showRLER (decRLER (mkRLER 3 'a'))" $
        showRLER (decRLER (mkRLER 3 'a'))
          @?= "aa"
    ]

consRLESTests =
  testGroup
    "Tests for consRLES"
    [ testCase "consRLES 'a' emptyRLES" $
        show (consRLES 'a' emptyRLES)
          @?= "\"a\"",
      testCase "showRLES $ consRLES 'a' emptyRLES" $
        showRLES (consRLES 'a' emptyRLES)
          @?= "(1,'a')",
      testCase "consRLES 'a' $ consRLES 'a' emptyRLES" $
        show
          ( consRLES 'a' $
              consRLES 'a' emptyRLES
          )
          @?= "\"aa\"",
      testCase "showRLES . consRLES 'a' $ consRLES 'a' emptyRLES" $
        showRLES
          ( consRLES 'a' $
              consRLES 'a' emptyRLES
          )
          @?= "(2,'a')",
      testCase "consRLES 'b' $ consRLES 'a' emptyRLES" $
        show
          ( consRLES 'b' $
              consRLES 'a' emptyRLES
          )
          @?= "\"ba\"",
      testCase "showRLES . consRLES 'b' $ consRLES 'a' emptyRLES" $
        showRLES
          ( consRLES 'b' $
              consRLES 'a' emptyRLES
          )
          @?= "(1,'b')(1,'a')"
    ]

toRLESTests =
  testGroup
    "Tests for toRLES"
    [ testCase "showRLES $ toRLES []" $
        showRLES (toRLES [])
          @?= "",
      testCase "showRLES $ toRLES \"a\"" $
        showRLES (toRLES "a")
          @?= "(1,'a')",
      testCase "showRLES $ toRLES \"aabbbabcc\"" $
        showRLES (toRLES "aabbbabcc")
          @?= "(2,'a')(3,'b')(1,'a')(1,'b')(2,'c')"
    ]

lengthRLESTests =
  testGroup
    "Tests for lengthRLES"
    [ testCase "lengthRLES $ toRLES []" $
        lengthRLES (toRLES [])
          @?= 0,
      testCase "lengthRLES $ toRLES \"abcd\"" $
        lengthRLES (toRLES "abcd")
          @?= 4,
      testCase "lengthRLES $ toRLES \"abbcccdddd\"" $
        lengthRLES (toRLES "abbcccdddd")
          @?= 10
    ]

headRLESTests =
  testGroup
    "Tests for headRLES"
    [ testCase "headRLES $ toRLES []" $
        headRLES (toRLES [])
          @?= Nothing,
      testCase "headRLES $ toRLES \"a\"" $
        headRLES (toRLES "a")
          @?= Just 'a',
      testCase "headRLES $ toRLES \"aabbbabcc\"" $
        headRLES (toRLES "aabbbabcc")
          @?= Just 'a'
    ]

tailRLESTests =
  testGroup
    "Tests for tailRLES"
    [ testCase "tailRLES $ toRLES []" $
        tailRLES (toRLES [])
          @?= Nothing,
      testCase "tailRLES $ toRLES \"a\"" $
        show <$> tailRLES (toRLES "a")
          @?= Just "\"\"",
      testCase "tailRLES $ toRLES \"aabbbabcc\"" $
        show <$> tailRLES (toRLES "aabbbabcc")
          @?= Just "\"abbbabcc\"",
      testCase "tailRLES $ toRLES \"abbbabcc\"" $
        show <$> tailRLES (toRLES "abbbabcc")
          @?= Just "\"bbbabcc\""
    ]

initRLESTests =
  testGroup
    "Tests for initRLES"
    [ testCase "initRLES $ toRLES []" $
        initRLES (toRLES [])
          @?= Nothing,
      testCase "initRLES $ toRLES \"a\"" $
        show <$> initRLES (toRLES "a")
          @?= Just "\"\"",
      testCase "initRLES $ toRLES \"aabbbabcc\"" $
        show <$> initRLES (toRLES "aabbbabcc")
          @?= Just "\"aabbbabc\"",
      testCase "initRLES $ toRLES \"aabbbabc\"" $
        show <$> initRLES (toRLES "aabbbabc")
          @?= Just "\"aabbbab\""
    ]

concatRLESTests =
  testGroup
    "Tests for (@++@)"
    [ testCase "showRLES $ toRLES \"aabb\" @++@ toRLES \"ccdd\"" $
        showRLES
          ( toRLES "aabb"
              @++@ toRLES "ccdd"
          )
          @?= "(2,'a')(2,'b')(2,'c')(2,'d')",
      testCase "showRLES $ toRLES \"aabb\" @++@ toRLES \"bbdd\"" $
        showRLES
          ( toRLES "aabb"
              @++@ toRLES "bbdd"
          )
          @?= "(2,'a')(4,'b')(2,'d')"
    ]

mapRLESTests =
  testGroup
    "Tests for mapRLES"
    [ testCase "toUpper" $
        showRLES (mapRLES toUpper $ toRLES "abbccc")
          @?= "(1,'A')(2,'B')(3,'C')",
      testCase "const 'A'" $
        showRLES (mapRLES (const 'A') $ toRLES "abbcccdddd")
          @?= "(10,'A')",
      testCase "\\x -> if x `L.elem` ['A'..'L'] then '0' else '1'" $
        showRLES
          ( mapRLES
              (\x -> if x `elem` ['A' .. 'L'] then '0' else '1')
              $ toRLES ['A' .. 'Z']
          )
          @?= "(12,'0')(14,'1')"
    ]

palindromeRLESTests =
  testGroup
    "Tests for palindromeRLES"
    [ testCase "palindromeRLES $ toRLES \"abbcccbba\"" $
        palindromeRLES (toRLES "abbcccbba")
          @?= True,
      testCase "palindromeRLES $ toRLES \"abbbaa\"" $
        palindromeRLES (toRLES "abbbaa")
          @?= False
    ]

takeRLESTests =
  testGroup
    "Tests for takeRLES"
    [ testCase "[(i, takeRLES i (toRLES \"abbcccdddd\")) | i <- [-1..11]]" $
        [ (i, show (takeRLES i (toRLES "abbcccdddd")))
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

dropRLESTests =
  testGroup
    "Tests for dropRLES"
    [ testCase "[(i, dropRLES i (toRLES \"abbcccdddd\")) | i <- [-1..11]]" $
        [ (i, show $ dropRLES i (toRLES "abbcccdddd"))
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

splitAtRLESTests =
  testGroup
    "Tests for splitAtRLES"
    [ testCase "[(i, splitAtRLES i (toRLES \"abbcccdddd\")) | i <- [-1..11]]" $
        [(i, join bimap show $ splitAtRLES i (toRLES "abbcccdddd")) | i <- [-1 .. 11]]
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

countRLESTests =
  testGroup
    "Tests for countRLESTests"
    [ testCase "countRLES 'a' $ toRLES \"abbcccdddabab\"" $
        countRLES 'a' (toRLES "abbcccdddabab")
          @?= 3,
      testCase "countRLES 'b' $ toRLES \"abbcccdddabab\"" $
        countRLES 'b' (toRLES "abbcccdddabab")
          @?= 4,
      testCase "countRLES 'e' $ toRLES \"abbcccdddabab\"" $
        countRLES 'e' (toRLES "abbcccdddabab")
          @?= 0
    ]

mapRLES'Tests =
  testGroup
    "Tests for mapRLES'"
    [ testCase "toUpper" $
        showRLES (mapRLES' toUpper $ toRLES "abbccc")
          @?= "(1,'A')(2,'B')(3,'C')",
      testCase "const 'A'" $
        showRLES (mapRLES' (const 'A') $ toRLES "abbcccdddd")
          @?= "(10,'A')",
      testCase "\\x -> if x `L.elem` ['A'..'L'] then '0' else '1'" $
        showRLES
          ( mapRLES'
              (\x -> if x `elem` ['A' .. 'L'] then '0' else '1')
              $ toRLES ['A' .. 'Z']
          )
          @?= "(12,'0')(14,'1')"
    ]

ex3Tests =
  testGroup
    "Tests for exercise 3"
    [ incRLERTests,
      decRLERTests,
      consRLESTests,
      toRLESTests,
      lengthRLESTests,
      headRLESTests,
      tailRLESTests,
      initRLESTests,
      concatRLESTests,
      mapRLESTests,
      palindromeRLESTests,
      takeRLESTests,
      dropRLESTests,
      splitAtRLESTests,
      countRLESTests,
      mapRLES'Tests
    ]