-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

-- To play:
-- ghci
-- :l sokoban
-- go

module Sokoban where
import System.IO
import Data.Char

-- list of list of characters (board spaces) (inner lists horizontal)
data State = Empty
             | State { board  :: Board
              , previousState:: State
              , player :: Player
              , level  :: String } deriving(Eq)

instance Show State where
  show (State board previousState player level) = 
    "Your Position: " ++ show player ++               -- print player position
    foldl (\acc row -> acc ++ "\n" ++ row) "" board   -- print formatted board

data Result = WonGame State
            | ContinueGame State
            | RestartGame State
            | UndoGame State
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
level1 = (ContinueGame (State board1 Empty player1 "1"))

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
level2 = (ContinueGame (State board2 Empty player2 "2"))

board3 = ["########",
          "#      #",
          "#  $  .#",
          "#@ $  .#",
          "#  $  .#",
          "#      #",
          "########"]
player3 = (3,1)
level3 = (ContinueGame (State board3 Empty player3 "3"))

board4 = ["########",
          "#  #.  #",
          "# $#   #",
          "#  # @##",
          "#  # $##",
          "#    .##",
          "########"]
player4 = (3,5)
level4 = (ContinueGame (State board4 Empty player4 "4"))

board5 = ["#########",
          "##  #   #",
          "#.$.  $ #",
          "# #  ## #",
          "# @$.$. #",
          "#########"]
player5 = (4,2)
level5 = (ContinueGame (State board5 Empty player5 "5"))

board6 = ["#########",
          "#  #   .#",
          "#@$ $   #",
          "# $ ##..#",
          "#   #####",
          "#########"]
player6 = (2,1)
level6 = (ContinueGame (State board6 Empty player6 "6"))

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

{-- Game Functions --}

-- main sokoban function
play :: Result -> IO()
play (ContinueGame s)= 
  do
    printBoard (board s)
    putStr "Input your next move (W,A,S,D):"
    move <- getLine 
    case move of
      [] -> play (ContinueGame s) -- continue if no input
      _ -> play (getNextBoard (Action (toUpper (move!!0))) s)      

play (WonGame s) =
  do
    printBoard (board s)
    putStrLn "Congratulations! You beat the level!"

play (QuitGame s) =
  do
    putStrLn "Exiting level..."

play (UndoGame (State board pS (r, c) l)) =
  do
    putStrLn "Switching to previous move..."
    if pS == Empty then (putStrLn "No previos move...")
    else play (ContinueGame pS)
     
play (RestartGame (State board pS (r, c) l))=
  do
    putStrLn "Restarting level..."
    playLevel l

-- takes user input and current state of board
-- returns the next state of board as a result (game won or continue game)
-- should be used as a helper in the main game function
getNextBoard :: Game 
getNextBoard move (State board pS (r,c) l)
  | move == (Action 'W') = movePlayer (State board (State board pS (r,c) l) (r,c) l) (r-1,c) (r-2,c)
  | move == (Action 'A') = movePlayer (State board (State board pS (r,c) l) (r,c) l) (r,c-1) (r,c-2)
  | move == (Action 'S') = movePlayer (State board (State board pS (r,c) l) (r,c) l) (r+1,c) (r+2,c)
  | move == (Action 'D') = movePlayer (State board (State board pS (r,c) l) (r,c) l) (r,c+1) (r,c+2)
  | move == (Action 'Q') = QuitGame (State board (State board pS (r,c) l) (r, c) l)
  | move == (Action 'R') = RestartGame (State board (State board pS (r,c) l) (r, c) l)
  | move == (Action 'U') = UndoGame (State board pS (r, c) l)
  | otherwise = ContinueGame (State board (State board pS (r,c) l) (r, c) l) -- return same state
    
movePlayer :: State -> Coordinates -> Coordinates -> Result
movePlayer (State board pS (pr, pc) l) (r1,c1) (r2,c2)
  | destination == '#' = ContinueGame (State board pS (pr,pc) l) -- can't move to a wall
  | destination == ' ' = ContinueGame (State (moveToUnoccupied board (pr,pc) (r1,c1)) pS (r1,c1) l)
  | destination == '.' = ContinueGame (State (moveToGoal board (pr,pc) (r1,c1)) pS (r1,c1) l)
  | destination == '$' = case behind_destination of
                              '#' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into wall
                              ' ' -> if (isGameWon (State (pushToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l))
                                         then WonGame (State (pushToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                                         else ContinueGame (State (pushToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '.' -> if (isGameWon (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l))
                                         then WonGame (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                                         else ContinueGame (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '$' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into another box
                              '*' -> ContinueGame(State board pS (pr,pc) l) -- "
  | destination == '*' = case behind_destination of
                              '#' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into wall
                              ' ' -> if (isGameWon (State (pushGoalToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l))
                                         then WonGame (State (pushGoalToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                                         else ContinueGame (State (pushGoalToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '.' -> if (isGameWon (State (pushGoalToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1)  l))
                                         then WonGame (State (pushGoalToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                                         else ContinueGame (State (pushGoalToGoal board  (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '$' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into another box
                              '*' -> ContinueGame (State board pS (pr,pc) l) -- "
  where destination = getCharacter board (r1,c1) -- character at destination cell
        behind_destination = getCharacter board (r2,c2)

playLevel :: String -> IO()
playLevel n =
  case n of
    "1" -> play level1 
    "2" -> play level2 
    "3" -> play level3 
    "4" -> play level4 
    "5" -> play level5 
    "6" -> play level6 
    _ -> putStrLn "Not a valid level..."

levelSelection :: IO()
levelSelection =
  do
    putStrLn ""
    putStrLn "Enter a level to play (1-6) or 'q' to exit the game:"
    level <- getLine
    case level of
      "q" -> return ()
      _ -> do      
        playLevel level
        levelSelection

go :: IO()
go =
  do
    putStrLn "------------------------"
    putStrLn "-- Welcome to Sokoban!--"
    putStrLn "------------------------"
    putStrLn ""
    putStrLn "INSTRUCTIONS"
    putStrLn "A level contains a grid of these elements:"
    putStrLn "Walls:                \'#\'"
    putStrLn "Boxes:                \'$\'"
    putStrLn "Goals:                \'.\'"
    putStrLn "Boxes on Goals:       \'*\'"
    putStrLn "Player:               \'@\'"
    putStrLn "Player on Goals:      \'+\'"
    putStrLn "Empty squares:        \' \'"
    putStrLn ""
    putStrLn "You can move your player (indicated by @ or +) using these controls:"
    putStrLn "W: Up"
    putStrLn "A: Left"
    putStrLn "S: Down"
    putStrLn "D: Right"
    putStrLn ""
    putStrLn "You can move around on any empty squares and push boxes as well."
    putStrLn "Your goal is to move all boxes onto the goals."
    putStrLn "The level is complete once all boxes are on the goals."
    putStrLn ""
    putStrLn "Press Q to quit the level at anytime, Press U to Undo and press R to restart."
    levelSelection

{-- Tests for functions
> getNextBoard (Action 'D') level1
State ["#########","# @ $  .#","#########"] (1,2)

> getNextBoard (Action 'D') (getNextBoard (Action 'D') level1)
State ["#########","#  @$  .#","#########"] (1,3)

> getNextBoard (Action 'A') level1
State ["#########","#@  $  .#","#########"] (1,1)

> getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') level1))
State ["#########","#   @$ .#","#########"] (1,4)

> getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') level1))))
State ["#########","#     @*#","#########"] (1,6)

> isGameWon level1
False

> isGameWon (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') (getNextBoard (Action 'D') level1)))))
True

--}

-- ?
-- passing the state instead of board and (pr,pc) separately to move & push functions?

-- TODO
-- arrow key input (only if there is time)
-- R for restart and U for undo instead of hint giving?