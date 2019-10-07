-- To run 
-- ghci
-- :load sokoban

data State = State Board
  deriving (Ord, Eq, Show)

type Board = [[Char]]

data Result = EndOfGame Char State
  | ContinueGame State

-- takes current state, inputted move, and returns next state
type Game = State -> Char -> Result


