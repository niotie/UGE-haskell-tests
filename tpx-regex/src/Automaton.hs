-- PART 2 – Simulating Automata

module Automaton
-- ( Automaton (..),
--   Input (..),
--   State,
--   TransTable,
--   accepts,
--   maxState,
--   mkTransTable,
--   exampleAutomaton1,
--   exampleAutomaton2,
--   exampleAutomaton3,
-- )
where

import Data.List qualified as L
import Data.Map qualified as M
import Numeric.Natural (Natural)
import Word
import Prelude hiding (Word)

type State = Natural

data Input = Epsilon | Symbol Letter
  deriving (Eq, Ord, Show)

type TransTable = M.Map Input (M.Map State [State])

type TransFunc = Input -> State -> [State]

data Automaton
  = Automaton
  { initState :: State,
    transTable :: TransTable,
    acceptStates :: [State]
  }
  deriving Eq

instance Show Automaton where
  show auto =
    "\n  Automaton\n"
      <> ("  ├── initState: " <> show (initState auto) <> "\n")
      <> ("  ├── transTable:\n" <> showTransTable (transTable auto))
      <> ("  └── acceptStates: " <> show (acceptStates auto) <> "\n")
    where
      showTransTable =
        L.concatMap
          ( \(input, partialTable) ->
              "  │     " <> show input <> " ->\n" <> showPartialTable partialTable
          )
          . M.toAscList
      showPartialTable =
        L.concatMap
          ( \(source, targets) ->
              "  │       " <> show source <> " -> " <> show targets <> "\n"
          )
          . M.toAscList

-- Deterministic automaton recognizing
-- {a^n | n % 2 = 0}
exampleAutomaton1 :: Automaton
exampleAutomaton1 =
  Automaton
    { initState = 0,
      transTable =
        mkTransTable
          [ (Symbol 'a', 0, 1),
            (Symbol 'a', 1, 0)
          ],
      acceptStates = [0]
    }

-- Nondeterministic automaton recognizing
-- {uabc | u ∈ {a,b,c}^*}
exampleAutomaton2 :: Automaton
exampleAutomaton2 =
  Automaton
    { initState = 0,
      transTable =
        mkTransTable
          [ (Symbol 'a', 0, 0),
            (Symbol 'a', 0, 1),
            (Symbol 'b', 0, 0),
            (Symbol 'b', 1, 2),
            (Symbol 'c', 0, 0),
            (Symbol 'c', 2, 3)
          ],
      acceptStates = [3]
    }

-- Nondeterministic automaton with ε-transitions recognizing
-- {a^m b^n | m,n ∈ ℕ} ∪ {b^m a^n | m,n ∈ ℕ}
exampleAutomaton3 :: Automaton
exampleAutomaton3 =
  Automaton
    { initState = 0,
      transTable =
        mkTransTable
          [ (Epsilon, 0, 1),
            (Epsilon, 0, 3),
            (Epsilon, 1, 2),
            (Epsilon, 3, 4),
            (Symbol 'a', 1, 1),
            (Symbol 'a', 4, 4),
            (Symbol 'b', 2, 2),
            (Symbol 'b', 3, 3)
          ],
      acceptStates = [2, 4]
    }

mkTransTable :: [(Input, State, State)] -> TransTable
mkTransTable = undefined

alphabet :: Automaton -> [Letter]
alphabet = undefined

states :: Automaton -> [State]
states = undefined

maxState :: Automaton -> State
maxState = undefined

transFunc :: Automaton -> TransFunc
transFunc = undefined

untilFixpoint :: (Eq a) => (a -> a) -> a -> a
untilFixpoint = undefined

epsilonClosure :: Automaton -> State -> [State]
epsilonClosure = undefined

reachedStates :: Automaton -> Word -> [State]
reachedStates = undefined

accepts :: Automaton -> Word -> Bool
accepts = undefined
