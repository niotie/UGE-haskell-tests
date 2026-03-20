module RegexTests where

import Test.Tasty
import Test.Tasty.HUnit as HU

import Regex

test_parseAtom =
  testGroup
    "parseAtom tests"
    [ testCase "empty string" $
        parseAtom "" @?= Left "Unexpected end of string",
      testCase "wrong first character" $
        parseAtom "*ab*" @?= Left "Expected '(', '0', '3', or a letter, got *",
      testCase "empty language" $
        parseAtom "0" @?= Right (EmptyLang, ""),
      testCase "empty word" $
        parseAtom "3|abc" @?= Right (EmptyWord, "|abc"),
      testCase "single letter" $
        parseAtom "a(ba)(#@!?%" @?= Right (Single 'a', "(ba)(#@!?%"),
      testCase "parenthesized atom (requires parseTerm)" $
        parseAtom "(ab*|c)rest" @?= Right 
          ( Union 
            ( Concat 
              ( Single 'a' ) 
              ( Star (Single 'b') ) ) 
            ( Single 'c'),
          "rest" )
    ]

test_parseFactor =
  testGroup
    "parseFactor tests"
    [ testCase "wrong first character" $
        parseFactor "*abc" @?= Left "Expected '(', '0', '3', or a letter, got *",
      testCase "single letter" $
        parseFactor "abc" @?= Right (Single 'a', "bc"),
      testCase "star factor" $
        parseFactor "a*bc" @?= Right (Star (Single 'a'), "bc")
    ]

test_parseTerm =
  testGroup
    "parseTerm tests"
    [ testCase "empty string" $
        parseTerm "" @?= Left "Unexpected end of string",
      testCase "two single letters" $
        parseTerm "ab#@!?%" @?= Right (Concat (Single 'a') (Single 'b'), "#@!?%"),
      testCase "complex term" $
        parseTerm "a*bc*|r|e|s|t"
          @?= Right (Concat (Star (Single 'a')) (Concat (Single 'b') (Star (Single 'c'))), "|r|e|s|t")
    ]

test_parseRegex =
  testGroup
    "parseRegex tests"
    [ 
      testCase "failure on empty string" $
        parseRegex "" @?= Left "Unexpected end of string",
      testCase "failure on hanging open parenthesis" $
        parseRegex "a(" @?= Left "Unexpected end of string",
      testCase "single letter and closing parenthesis" $
        parseRegex "a)" @?= Right (Single 'a', ")"),
      testCase "union of single letters" $
        parseRegex "a|b" @?= Right (Union (Single 'a') (Single 'b'), ""),
      testCase "complex regex" $
        parseRegex "((a|b)*c)*|de*f#@!?%"
          @?= Right
            ( Union
                ( Star
                    ( Concat
                        ( Star
                            ( Union (Single 'a') (Single 'b') ) )
                        ( Single 'c' ) ) )
                ( Concat
                    ( Single 'd' )
                    ( Concat
                        ( Star ( Single 'e' ) )
                        ( Single 'f' ) ) ),
              "#@!?%"
            )
    ]

test_toRegex =
  testGroup
    "toRegex tests"
    [ 
      testCase "failure on empty string" $
        toRegex "" @?= Left "Unexpected end of string",
      testCase "failure on hanging open parenthesis" $
        toRegex "a(" @?= Left "Unexpected end of string",
      testCase "non-empty suffix (closing parenthesis)" $
        toRegex "a)" @?= Left "Unexpected suffix : )",
      testCase "non-empy suffix (lots of stuff)" $
        toRegex "a#@!?%" @?= Left "Unexpected suffix : #@!?%",
      testCase "complex regex 1" $
        toRegex "(a*b|c)d"
          @?= Right
            ( Concat
              ( Union
                ( Concat
                  ( Star ( Single 'a') )
                  ( Single 'b' ) )
                ( Single 'c' ) )
              ( Single 'd' )
            ),
      testCase "complex regex 2" $
        toRegex "((a|b)*c)*|de*f"
          @?= Right
            ( Union
                ( Star
                    ( Concat
                        ( Star
                            ( Union (Single 'a') (Single 'b') ) )
                        ( Single 'c' ) ) )
                ( Concat
                    ( Single 'd' )
                    ( Concat
                        ( Star ( Single 'e' ) )
                        ( Single 'f' ) ) )
            )
    ]