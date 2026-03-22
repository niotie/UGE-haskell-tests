-- PART 3 – From Regular Expressions to Automata

module Thompson
-- ( match,
-- )
where

import Automaton
import Regex
import Result
import Word
import Prelude hiding (Word, concat)

emptyAutomaton :: Automaton
emptyAutomaton = undefined

epsilonAutomaton :: Automaton
epsilonAutomaton = undefined

letterAutomaton :: Letter -> Automaton
letterAutomaton = undefined

merge :: TransTable -> TransTable -> TransTable
merge = undefined

mapToStates :: (State -> State) -> Automaton -> Automaton
mapToStates = undefined

concat :: Automaton -> Automaton -> Automaton
concat = undefined

union :: Automaton -> Automaton -> Automaton
union = undefined

star :: Automaton -> Automaton
star = undefined

toAutomaton :: Regex -> Automaton
toAutomaton = undefined

match :: String -> Word -> Result Bool
match = undefined
