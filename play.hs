-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

-- To play:
-- ghci
-- :l play
-- go

module Play where
import System.IO
import Data.Char
import Sokoban
import Hints

-- [TODO]: ?
-- passing the state instead of board and (pr,pc) separately to move & push functions?

{-- Game Functions --}

-- main play function
play :: Result -> [Coordinates] -> IO()
play (ContinueGame s) deadsquares =
  do
    printBoard (board s)
    putStr "Input your next move (W,A,S,D):"
    move <- getLine 
    case move of
      [] -> play (ContinueGame s) deadsquares -- continue if no input
      _ -> play (getNextBoard (Action (toUpper (move!!0))) s deadsquares) deadsquares      

play (WonGame s) _ =
  do
    printBoard (board s)
    putStrLn "Congratulations! You beat the level!"

play (QuitGame s) _ =
  do
    putStrLn "Exiting level..."

play (UndoGame (State board pS (r, c) l)) deadsquares =
  do
    putStrLn "Switching to previous move..."
    if pS == Empty then (putStrLn "No previous move...")
    else play (ContinueGame pS) deadsquares
     
play (RestartGame (State board pS (r, c) l)) _ =
  do
    putStrLn "Restarting level..."
    playLevel l

-- takes user input and current state of board
-- returns the next state of board as a result (game won or continue game)
-- should be used as a helper in the main game function
getNextBoard :: Action -> State -> [Coordinates] -> Result
getNextBoard move (State board pS (r,c) l) deadsquares
  | move == (Action 'W') = movePlayer (State board newPrevState (r,c) l) (r-1,c) (r-2,c)
  | move == (Action 'A') = movePlayer (State board newPrevState (r,c) l) (r,c-1) (r,c-2)
  | move == (Action 'S') = movePlayer (State board newPrevState (r,c) l) (r+1,c) (r+2,c)
  | move == (Action 'D') = movePlayer (State board newPrevState (r,c) l) (r,c+1) (r,c+2)
  | move == (Action 'H') = if (isGameWon (giveHint (State board pS (r,c) l) deadsquares))
                               then WonGame (giveHint (State board pS (r,c) l) deadsquares)
                               else ContinueGame (giveHint (State board pS (r,c) l) deadsquares)
  | move == (Action 'Q') = QuitGame (State board newPrevState (r, c) l)
  | move == (Action 'R') = RestartGame (State board newPrevState (r, c) l)
  | move == (Action 'U') = UndoGame (State board pS (r, c) l)
  | otherwise = ContinueGame (State board newPrevState (r, c) l) -- return same state
  where newPrevState = (State board pS (r,c) l)
    
movePlayer :: State -> Coordinates -> Coordinates -> Result
movePlayer (State board pS (pr, pc) l) (r1,c1) (r2,c2)
  | destination == '#' = ContinueGame (State board pS (pr,pc) l) -- can't move to a wall
  | destination == ' ' = ContinueGame (State (moveToUnoccupied board (pr,pc) (r1,c1)) pS (r1,c1) l)
  | destination == '.' = ContinueGame (State (moveToGoal board (pr,pc) (r1,c1)) pS (r1,c1) l)
  | destination == '$' = case behind_destination of
                              '#' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into wall
                              ' ' -> ContinueGame (State (pushToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '.' -> if (isGameWon (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l))
                                         then WonGame (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                                         else ContinueGame (State (pushToGoal board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '$' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into another box
                              '*' -> ContinueGame(State board pS (pr,pc) l) -- "
  | destination == '*' = case behind_destination of
                              '#' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into wall
                              ' ' -> ContinueGame (State (pushGoalToUnoccupied board (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '.' -> ContinueGame (State (pushGoalToGoal board  (pr,pc) (r1,c1) (r2,c2)) pS (r1,c1) l)
                              '$' -> ContinueGame (State board pS (pr,pc) l) -- can't push box into another box
                              '*' -> ContinueGame (State board pS (pr,pc) l) -- "
  where destination = getCharacter board (r1,c1) -- character at destination cell
        behind_destination = getCharacter board (r2,c2)

playLevel :: String -> IO()
playLevel n =
  case n of
    "1" -> play (ContinueGame level1) (findDeadSquares board1) -- finds dead squares on level load
    "2" -> play (ContinueGame level2) (findDeadSquares board2)
    "3" -> play (ContinueGame level3) (findDeadSquares board3)
    "4" -> play (ContinueGame level4) (findDeadSquares board4)
    "5" -> play (ContinueGame level5) (findDeadSquares board5)
    "6" -> play (ContinueGame level6) (findDeadSquares board6)
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
    putStrLn "-------------------------"
    putStrLn "-- Welcome to Sokoban! --"
    putStrLn "-------------------------"
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
> getNextBoard (Action 'D') level1 []
Your Position: (1,2)
#########
# @ $  .#
#########

-- Test for multiple getNextBoard no longer work due to use of Result type. 

--}