-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, JeongSoo Doo, Gurveer Aulakh

-- https://wiki.ubc.ca/Course:CPSC312-2019-Sokoban


module Sokoban where

import System.IO

data State = State [[Char]] Player          -- list of list of characters (board spaces) (inner lists horizontal)
        deriving (Ord, Eq, Show)

        -- TODO figure out how to format print state

data Result = WonGame State
            | ContinueGame State
        deriving (Eq, Show)

type Game = Action -> State -> Result
type Coordinates = (Int, Int)               -- first coordinate is the row
                                            -- second coordinate is the column
type Player = Coordinates                   -- 

data Action = Action Char                   -- 'W', 'A', 'S', 'D' as input for up/left/down/right respectively (and possibly other actions such as UNDO and RESTART)
        deriving (Eq)


-- Level starting states
-- Levels provided by http://www.sokobano.de/wiki/index.php?title=How_to_play_Sokoban
{-
  # : wall
  $ : box
  . : goal
    : unoccupied space
  @ : the player
  * : box on goal
  + : player on goal

-}
level1 = (State 
          ["#########",
          "#@  $  .#",
          "#########"] 
          (1,1))

level2 = (State 
          ["########",
          "#    ###",
          "#@$  ###",
          "#### ###",
          "##   ###",
          "##    ##",
          "#  ##. #",
          "#      #",
          "#####  #",
          "########"],
          (2,1))

level3 = (State 
        ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]
          (3,1))


-- takes user input and current state of board
-- returns the next state
updateBoard :: Game
updateBoard move (State board (x, y))
  | move == (Action 'W') = playerMove (State board (x,y)) (x-1,y) (x-2,y)
  | move == (Action 'A') =  playerMove (State board (x,y)) (x,y-1) (x,y-2)
  | otherwise = ContinueGame (State board (x, y))
    
playerMove :: State -> Coordinates -> Coordinates -> Result
playerMove (State board (x, y)) (r1,c1) (r2,c2)
  | ((board !! r1) !! c1) == ' ' = ContinueGame (State board (r1,c1))
  | ((board !! r1) !! c1) == '#' = ContinueGame (State board (x,y))
  | otherwise = ContinueGame (State board (x,y))

-- Tests for functions



-- 


-- Variables
-- playerlocation :: (Int, Int)
-- board :: [[Char]]

-- Showing the current board (State)?

-- Check move validity
{-
Allowed moves:
    Player '@' moves up/right/down/left to an adjacent unoccupied space ' '.
    Player '@' pushes a box '$' up/right/down/left one space to an adjacent unoccupied space ' ' or goal '.'. 
-}

-- Game end
{-
All goals are occupied by a box ('*'), or no '.' AND no '+' on the board. 
-}

-- Menu
-- load level

-- I/O
-- R for restart and U for undo?
