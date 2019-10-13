-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

-- To play:
-- ghci
-- :l sokoban
-- go

module Play where
import System.IO
import Data.Char
import Sokoban
import Hints

{-- Game Functions --}

-- main sokoban function
play :: Result -> IO()
play (ContinueGame s) = 
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

-- takes user input and current state of board
-- returns the next state of board as a result (game won or continue game)
-- should be used as a helper in the main game function
getNextBoard :: Game
getNextBoard move (State board (r,c))
  | move == (Action 'W') = movePlayer (State board (r,c)) (r-1,c) (r-2,c)
  | move == (Action 'A') =  movePlayer (State board (r,c)) (r,c-1) (r,c-2)
  | move == (Action 'S') = movePlayer (State board (r,c)) (r+1,c) (r+2,c)
  | move == (Action 'D') = movePlayer (State board (r,c)) (r,c+1) (r,c+2)
  | move == (Action 'Q') = QuitGame (State board (r, c))
  | otherwise = ContinueGame (State board (r, c)) -- return same state
    
movePlayer :: State -> Coordinates -> Coordinates -> Result
movePlayer (State board (pr, pc)) (r1,c1) (r2,c2)
  | destination == '#' = ContinueGame (State board (pr,pc)) -- can't move to a wall
  | destination == ' ' = ContinueGame (State (moveToUnoccupied board (pr,pc) (r1,c1)) (r1,c1))
  | destination == '.' = ContinueGame (State (moveToGoal board (pr,pc) (r1,c1)) (r1,c1))
  | destination == '$' = case behind_destination of
                              '#' -> ContinueGame (State board (pr,pc)) -- can't push box into wall
                              ' ' -> ContinueGame (State (pushToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) (r1,c1))
                              '.' -> if (isGameWon (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) (r1,c1)))
                                         then WonGame (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) (r1,c1))
                                         else ContinueGame (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) (r1,c1))
                              '$' -> ContinueGame (State board (pr,pc)) -- can't push box into another box
                              '*' -> ContinueGame(State board (pr,pc)) -- "
  | destination == '*' = case behind_destination of
                              '#' -> ContinueGame (State board (pr,pc)) -- can't push box into wall
                              ' ' -> ContinueGame (State (pushGoalToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) (r1,c1))
                              '.' -> ContinueGame (State (pushGoalToGoal board (pr,pc) (r1,c1) (r2,c2)) (r1,c1))
                              '$' -> ContinueGame (State board (pr,pc)) -- can't push box into another box
                              '*' -> ContinueGame (State board (pr,pc)) -- "
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
    putStrLn "║  Welcome to Sokoban! ║"
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
    putStrLn "Press Q to quit the level at anytime." -- and press R to restart
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
-- R for restart and U for undo