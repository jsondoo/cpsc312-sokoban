-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, JeongSoo Doo, Gurveer Aulakh

-- https://wiki.ubc.ca/Course:CPSC312-2019-Sokoban


module Sokoban where

import System.IO

data State = State Board Player          -- list of list of characters (board spaces) (inner lists horizontal)
        deriving (Ord, Eq, Show)

        -- TODO figure out how to format print state

data Result = WonGame State
            | ContinueGame State
        deriving (Eq, Show)

type Board = [[Cell]]
type Cell = (Coordinates, Char)
type Game = Action -> State -> Result
type Coordinates = (Int, Int)               -- first coordinate is the row
                                            -- second coordinate is the column
type Player = Coordinates                 

data Action = Action Char                   -- 'W', 'A', 'S', 'D' as input for up/left/down/right respectively (and possibly other actions such as UNDO and RESTART)
        deriving (Eq)

{-- Levels --}
-- provided by http://www.sokobano.de/wiki/index.php?title=How_to_play_Sokoban
{--
  # : wall
  $ : box
  . : goal
    : unoccupied space
  @ : the player
  * : box on goal
  + : player on goal
--}

board1 = ["#########",
          "#@  $  .#",
          "#########"]
player1 = (1,1)
level1 = (State (createBoard board1) player1)

board2 = ["########",
          "#    ###",
          "#@$  ###",
          "#### ###",
          "##   ###",
          "##    ##",
          "#  ##. #",
          "#      #",
          "#####  #",
          "########"]
player2 = (2,1)
level2 = (State (createBoard board2) player2)

board3 = ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]
player3 = (3,1)
level3 = (State (createBoard board3) player3)


{-- Helper functions --}
-- creates a board from list of strings
createBoard :: [[Char]] -> Board
createBoard board = map (\(r, rowOfCells) -> zip (zip (repeat r) [0..]) rowOfCells) (zip [0..] board)

-- returns character of a board at coordinates (r,c)
getCharacter :: Board -> Coordinates -> Char
getCharacter board (r,c) = snd ((board !! r) !! c)

printRow :: [Cell] -> IO()
printRow cells = putStrLn $ foldr (\(co,ch) acc -> ch:acc) "" cells

printBoard :: Board -> IO() 
printBoard = mapM_ printRow
-- usage: > printboard $ createBoard board1



{-- Game Functions --}

-- takes user input and current state of board
-- returns the next state
updateBoard :: Game
updateBoard move (State board (r,c))
  | move == (Action 'W') = playerMove (State board (r,c)) (r-1,c) (r-2,c)
  | move == (Action 'A') =  playerMove (State board (r,c)) (r,c-1) (r,c-2)
  | move == (Action 'S') = playerMove (State board (r,c)) (r+1,c) (r+2,c)
  | move == (Action 'D') = playerMove (State board (r,c)) (r,c+1) (r,c+2)
  | otherwise = ContinueGame (State board (r, c))
    
playerMove :: State -> Coordinates -> Coordinates -> Result
playerMove (State board (x, y)) (r1,c1) (r2,c2)
  | c == ' ' = ContinueGame (State board (r1,c1))
  | c == '#' = ContinueGame (State board (x,y)) 
  | c == '.' = ContinueGame (State board (x,y)) -- TODO implement
  | c == '*' = ContinueGame (State board (x,y)) -- boxes have lot of edge cases
  | c == '$' = ContinueGame (State board (x,y)) -- should also check if game is solved, after touching boxes
  | otherwise = ContinueGame (State board (x,y))
  where c = getCharacter board (x,y)



{-- Tests for functions

> updateBoard (Action 'W') level1
ContinueGame (State ["#########",
                     "#@  $  .#",
                     "#########"] (1,1))



--}


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
