-- PART 1 – Parsing Regular Expressions

module Regex
  ( Regex (..),
    toRegex,
    -- Internal functions
    -- (exported for testing)
    parseAtom,
    parseFactor,
    parseTerm,
    parseRegex,
  )
where

import Result
import Word

data Regex
  = EmptyLang
  | EmptyWord
  | Single Letter
  | Concat Regex Regex
  | Union Regex Regex
  | Star Regex
  deriving (Eq)

instance Show Regex where
  show = (<> "\n") . show' []
    where
      show' :: [Bool] -> Regex -> String
      show' flags regex = case regex of
        EmptyLang ->
          prefix flags <> "EmptyLang"
        EmptyWord ->
          prefix flags <> "EmptyWord"
        Single letter ->
          prefix flags <> "Single '" <> [letter] <> "'"
        Concat regex1 regex2 ->
          prefix flags
            <> "Concat"
            <> show' (flags <> [True]) regex1
            <> show' (flags <> [False]) regex2
        Union regex1 regex2 ->
          prefix flags
            <> "Union"
            <> show' (flags <> [True]) regex1
            <> show' (flags <> [False]) regex2
        Star regex1 ->
          prefix flags
            <> "Star"
            <> show' (flags <> [False]) regex1

      prefix :: [Bool] -> String
      prefix = ("\n  " <>) . prefix'
        where
          prefix' [] = ""
          prefix' [flag] =
            if flag then "├── " else "└── "
          prefix' (flag : rest) =
            (if flag then "│   " else "    ") <> prefix' rest

-- Grammar for regular expressions:
-- -----------------------------------------------
-- Regex  -> Term '|' Regex | Term
-- Term   -> Factor Term    | Factor
-- Factor -> Atom '*'       | Atom
-- Atom   -> '(' Regex ')'  | '0' | '3' | <letter>
-- -----------------------------------------------
-- '0' respresents EmptyLang and '3' represents EmptyWord.

parseAtom :: String -> Result (Regex, String)
parseAtom = undefined

parseFactor :: String -> Result (Regex, String)
parseFactor = undefined

parseTerm :: String -> Result (Regex, String)
parseTerm = undefined

parseRegex :: String -> Result (Regex, String)
parseRegex = undefined

toRegex :: String -> Result Regex
toRegex = undefined
