module Ants where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

import Ants.Types
import Hatch

-- | Uncomment these two lines once you get to Exercise 6.
-- import Data.Set (Set)
-- import qualified Data.Set as Set



{-| 
  Ex. 1: Implement a Show instance for LineState.

  (The pipe character and full block character are | and █)

  [JUSTIFY]
-}
instance Show LineState where
  show :: LineState -> String
  show = error "Not implemented"



{-|
  Ex. 2: Implement ruleX, which turns a cell on if it has exactly one neighbour which was on in the previous step. The three arguments are the states of the left neighbour, current cell, and right neighbour respectively.

  [JUSTIFY]
-}

ruleX :: CellState -> CellState -> CellState -> CellState
ruleX = error "Not implemented"



{-|
  Ex. 3: Implement applyRule, which, given a rule and a line state, applies the rule to each cell in the state and returns the updated state.

  [JUSTIFY]
-}
applyRule :: (CellState -> CellState -> CellState -> CellState)
          -> LineState
          -> LineState
applyRule = error "Not implemented"



{-|
  Ex. 4: Implement the loopedAt function, which takes a rule and a starting configuration, and returns the number of the iteration at which the automaton first revisits a state.

  [JUSTIFY]
-}
loopedAt :: (CellState -> CellState -> CellState -> CellState)
  -> LineState
  -> Int
loopedAt = error "Not implemented"



{-|
  Ex. 5: Implement allRules, which returns all 256 possible rules.

  [JUSTIFY]
-}
allRules :: [ CellState -> CellState -> CellState -> CellState ]
allRules = error "Not implemented"

{-|
  Ex. 6: Implement initialState, which returns the initial configuration of Langton's Ant.
-}
initialState :: AntState
initialState = error "Not implemented"



{-|
  Ex. 7: Define the functions leftOf and rightOf, which return the direction to the left and right of the given direction, respectively. Follow the additional constraints given in the specification, and answer the written question here.

  [JUSTIFY]

-}
leftOf :: Direction -> Direction
leftOf = error "Not implemented"

rightOf :: Direction -> Direction
rightOf = error "Not implemented"



{-|
  Ex. 8: Implement the step function, which takes the ant state and applies the logic given in the specification to return the new ant state.

  [JUSTIFY]
-}
step :: AntState -> AntState
step = error "Not implemented"



{-|
  Ex. 9: Visualise the behaviour of Langton's Ant by implementing the "animation" function. It takes a number (the step) and you must return a picture that represents the state of the automaton at that step.

  There are no tests for this. You can use `stack run` to test your implementation.

  [JUSTIFY]
-}
animation :: Int -> Image
animation = error "Not implemented"