-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

module Sokoban where

{- Types -}

-- list of list of characters (board spaces) (inner lists horizontal)
data State = Empty
             | State { board  :: Board
              , previousState:: State
              , player :: Player
              , level  :: String }

instance Eq State where
  x == y = board x == board y

instance Show State where
  show (State board previousState player level) = 
    "Your Position: " ++ show player ++               -- print player position
    foldl (\acc row -> acc ++ "\n" ++ row) "" board   -- print formatted board
    ++ "\n"

data Result = WonGame State
            | ContinueGame State
            | RestartGame State
            | UndoGame State
            | QuitGame State
  deriving (Eq, Show)

type Board = [[Char]]

type Coordinates = (Int, Int)               -- first coordinate is the row
                                            -- second coordinate is the column
type Player = Coordinates                 

data Action = Action Char                   -- W,A,S,D for up/down/left/right movement & H,U,Q
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
level1 = (State board1 Empty player1 "1")

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
level2 = (State board2 Empty player2 "2")

board3 = ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]
player3 = (3,1)
level3 = (State board3 Empty player3 "3")

board4 = ["########",
          "#  #.  #",
          "# $#   #",
          "#  # @##",
          "#  # $##",
          "#    .##",
          "########"]
player4 = (3,5)
level4 = (State board4 Empty player4 "4")

board5 = ["#########",
          "##  #   #",
          "#.$.  $ #",
          "# #  ## #",
          "# @$.$. #",
          "#########"]
player5 = (4,2)
level5 = (State board5 Empty player5 "5")

board6 = ["#########",
          "#  #   .#",
          "#@$ $   #",
          "# $ ##..#",
          "#   #####",
          "#########"]
player6 = (2,1)
level6 = (State board6 Empty player6 "6")

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
isGameWon (State board previousState (pr,pc) level) = (getCharacter board (pr,pc) /= '+') && not (foldr (\r b -> b || (elem '.' r)) False board)

{-- Tests for functions
> isGameWon level1
False

> isGameWon (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') level1)))))
True
--}