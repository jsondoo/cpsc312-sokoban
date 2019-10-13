-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

module Sokoban where

{- Types -}

data State = State          -- list of list of characters (board spaces) (inner lists horizontal)
    { board  :: Board
    , player :: Player }
    deriving (Eq)

instance Show State where
  show (State board player) = 
    "Your Position: " ++ show player ++               -- print player position
    foldl (\acc row -> acc ++ "\n" ++ row) "" board   -- print formatted board
    ++ "\n"

data Result = WonGame State
            | ContinueGame State
            | QuitGame State

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
level1 = (ContinueGame (State board1 player1))

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
level2 = (ContinueGame (State board2 player2))

board3 = ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]
player3 = (3,1)
level3 = (ContinueGame (State board3 player3))

board4 = ["########",
          "#  #.  #",
          "# $#   #",
          "#  # @##",
          "#  # $##",
          "#    .##",
          "########"]
player4 = (3,5)
level4 = (ContinueGame (State board4 player4))

board5 = ["#########",
          "##  #   #",
          "#.$.  $ #",
          "# #  ## #",
          "# @$.$. #",
          "#########"]
player5 = (4,2)
level5 = (ContinueGame (State board5 player5))

board6 = ["#########",
          "#  #   .#",
          "#@$ $   #",
          "# $ ##..#",
          "#   #####",
          "#########"]
player6 = (2,1)
level6 = (ContinueGame (State board6 player6))

{-- Helper functions --}
-- returns character of a board at coordinates (r,c)
getCharacter :: Board -> Coordinates -> Char
getCharacter board (r,c) = ((board !! r) !! c)

printRow :: [Char] -> IO()
printRow cells = putStrLn cells

printBoard :: Board -> IO() 
printBoard = mapM_ printRow
-- usage: > printBoard board1

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

-- given board, player coords, box coords, and box destination coords (being unoccupied space),
-- returns next board state
pushToUnoccupied :: Board -> Coordinates -> Coordinates -> Coordinates -> Board
pushToUnoccupied b (pr,pc) (br,bc) (r,c)
  | player == '@' = (writeToCell (writeToCell (writeToCell b (pr,pc) ' ') (br,bc) '@') (r,c) '$')
  | player == '+' = (writeToCell (writeToCell (writeToCell b (pr,pc) '.') (br,bc) '@') (r,c) '$')
  where player = getCharacter b (pr,pc)

-- given board, player coords, box coords, and box destination coords (being a goal),
-- returns next board state
pushToGoal :: Board -> Coordinates -> Coordinates -> Coordinates -> Board
pushToGoal b (pr,pc) (br,bc) (r,c)
  | player == '@' = (writeToCell (writeToCell (writeToCell b (pr,pc) ' ') (br,bc) '@') (r,c) '*')
  | player == '+' = (writeToCell (writeToCell (writeToCell b (pr,pc) '.') (br,bc) '@') (r,c) '*')
  where player = getCharacter b (pr,pc)

-- given board, player coords, box-on-goal coords, and box destination coords (being unoccupied space),
-- returns next board state
pushGoalToUnoccupied :: Board -> Coordinates -> Coordinates -> Coordinates -> Board
pushGoalToUnoccupied b (pr,pc) (br,bc) (r,c)
  | player == '@' = (writeToCell (writeToCell (writeToCell b (pr,pc) ' ') (br,bc) '+') (r,c) '$')
  | player == '+' = (writeToCell (writeToCell (writeToCell b (pr,pc) '.') (br,bc) '+') (r,c) '$')
  where player = getCharacter b (pr,pc)

-- given board, player coords, box-on-goal coords, and box destination coords (being a goal),
-- returns next board state
pushGoalToGoal :: Board -> Coordinates -> Coordinates -> Coordinates -> Board
pushGoalToGoal b (pr,pc) (br,bc) (r,c)
  | player == '@' = (writeToCell (writeToCell (writeToCell b (pr,pc) ' ') (br,bc) '+') (r,c) '*')
  | player == '+' = (writeToCell (writeToCell (writeToCell b (pr,pc) '.') (br,bc) '+') (r,c) '*')
  where player = getCharacter b (pr,pc)

-- given board,
-- returns True if given state is a winning state, false otherwise
-- (no '.' on board and player is not on a goal ('+'))
isGameWon :: State -> Bool
isGameWon (State board (pr,pc)) = (getCharacter board (pr,pc) /= '+') && not (foldr (\r b -> b || (elem '.' r)) False board)
