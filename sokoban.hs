-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

module Sokoban where
import System.IO

data State = State Board Player          -- list of list of characters (board spaces) (inner lists horizontal)

instance Show State where
  show (State board player) = 
    "Your Position: " ++ show player ++               -- print player position
    foldl (\acc row -> acc ++ "\n" ++ row) "" board   -- print formatted board

data Result = WonGame State
            | ContinueGame State

type Board = [[Char]]
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
level1 = (State board1 player1)

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
level2 = (State board2 player2)

board3 = ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]
player3 = (3,1)
level3 = (State board3 player3)


{-- Helper functions --}
-- returns character of a board at coordinates (r,c)
getCharacter :: Board -> Coordinates -> Char
getCharacter board (r,c) = ((board !! r) !! c)

printRow :: [Char] -> IO()
printRow cells = putStrLn cells

printBoard :: Board -> IO() 
printBoard = mapM_ printRow
-- usage: > printboard board1

-- given a board, some coordinates, and a character
-- returns a new board with the coordinates being written with given character
-- source: https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell
writeToCell :: Board -> Coordinates -> Char -> Board
writeToCell b (r,c) x =
  take r b ++
  [take c (b !! r) ++ [x] ++ drop (c + 1) (b !! r)] ++
  drop (r + 1) b

-- given board, player coordinates, destination coordinates (being unoccupied space),
-- returns next board state
moveToUnoccupied :: Board -> Coordinates -> Coordinates -> Board
moveToUnoccupied b (pr,pc) (r,c) 
  | player == '@' = (writeToCell (writeToCell b (pr,pc) ' ') (r,c) '@')
  | player == '+' = (writeToCell (writeToCell b (pr,pc) '.') (r,c) '@')
  where player = getCharacter b (pr,pc)

-- given board, player coordinates, destination coordinates (being a goal),
-- returns next board state
moveToGoal :: Board -> Coordinates -> Coordinates -> Board
moveToGoal b (pr,pc) (r,c) 
  | player == '@' = (writeToCell (writeToCell b (pr,pc) ' ') (r,c) '+')
  | player == '+' = (writeToCell (writeToCell b (pr,pc) '.') (r,c) '+')
  where player = getCharacter b (pr,pc)

{-- Game Functions --}

-- takes user input and current state of board
-- returns the next state of board
-- should be used as a helper in the main game function
getNextBoard :: Action -> State -> State
getNextBoard move (State board (r,c))
  | move == (Action 'W') = movePlayer (State board (r,c)) (r-1,c) (r-2,c)
  | move == (Action 'A') =  movePlayer (State board (r,c)) (r,c-1) (r,c-2)
  | move == (Action 'S') = movePlayer (State board (r,c)) (r+1,c) (r+2,c)
  | move == (Action 'D') = movePlayer (State board (r,c)) (r,c+1) (r,c+2)
  | otherwise = (State board (r, c)) -- return same state
    
movePlayer :: State -> Coordinates -> Coordinates -> State
movePlayer (State board (pr, pc)) (r1,c1) (r2,c2)
  | destination == '#' = (State board (pr,pc)) -- can't move to a wall
  | destination == ' ' = (State (moveToUnoccupied board (pr,pc) (r1,c1)) (r1,c1))
  | destination == '.' = (State (moveToGoal board (pr,pc) (r1,c1)) (r1,c1)) 
  | destination == '$' = (State board (pr,pc)) -- TODO
  | destination == '*' = (State board (pr,pc)) -- TODO
  | otherwise = (State board (pr,pc))
  where destination = getCharacter board (r1,c1) -- character at destination cell
        behind_destination = getCharacter board (r2,c2)


{-- Tests for functions
> getNextBoard (Action 'D') level1
State ["#########","# @ $  .#","#########"] (1,2)

> getNextBoard (Action 'D') (getNextBoard (Action 'D') level1)
State ["#########","#  @$  .#","#########"] (1,3)

> getNextBoard (Action 'A') level1
State ["#########","#@ $  .#","#########"] (1,1)



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
