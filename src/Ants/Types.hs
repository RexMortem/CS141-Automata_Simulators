module Ants.Types where

import Data.Set (Set)

--------------------------------------------------------------------------------
-- Type Definitions
-- For the Line Automaton:

data CellState = Off | On
  deriving (Eq, Ord, Show, Read)

newtype LineState = LS [CellState]
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Type Definitions
-- For Langton's Ant:

data Direction = North | East | South | West
  deriving (Eq, Ord, Show, Read)

data AntState = AS Direction (Int, Int) (Set (Int,Int))
  deriving (Eq, Ord, Show)