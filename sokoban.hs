-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, JeongSoo Doo, Gurveer Aulakh

-- https://wiki.ubc.ca/Course:CPSC312-2019-Sokoban


module Sokoban where

import System.IO

data State = State [[Char]] Player          -- list of list of characters (board spaces) (inner lists horizontal)
        deriving (Ord, Eq, Show)

data Result = WonGame State
            | ContinueGame State
        deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = (Int, Int)                    -- coordinates of player character on the board

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
level1 = ["#########",
          "#@  $  .#",
          "#########"]

level2 = ["########",
          "#    ###",
          "#@$  ###",
          "#### ###",
          "##   ###",
          "##    ##",
          "#  ##. #",
          "#      #",
          "#####  #",
          "########"]

level3 = ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]


sokoban :: Game
sokoban move (State board (x, y))
    | move == 'W'
    | move == 'A'
    | move == 'S'
    | move == 'D'
    



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