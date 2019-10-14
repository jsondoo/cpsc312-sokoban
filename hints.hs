-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

module Hints where
import Data.Maybe
import Sokoban

type Solution = [Hint]

data Hint = Hint (State, State) -- the states before and after a suggested box push
  deriving (Eq)

instance Show Hint where
    show (Hint (s1, s2)) = 
        "\nHINT\n" ++ 
        "Before Push:\n" ++ show s1 ++ "\n\n" ++ "After Push:\n" ++ show s2

type Move = (State, Coordinates)              -- state and dest
type Push = (State, Coordinates, Coordinates) -- state, box coords, and dest

data DeadSquare = DeadSquare -- a potential dead square
    { coords :: Coordinates
    , up     :: Bool         -- True if wall (#) above square
    , down   :: Bool         --                  below
    , left   :: Bool         --                  to the left of
    , right  :: Bool         --                         right
    }
  deriving (Show)

{-- Helper functions --}

-- given state, list of moves to try, and list of visited states,
-- returns list of reachable states with player adjacent to a box (moving only to unoccupied ' ' or goal '.' spaces)
reachableBoxes :: State -> [Move] -> [State] -> [State]
reachableBoxes (State b (pr,pc)) todo visited
    | isByBox (State b (pr,pc))      = (State b (pr,pc)) : (reachableBoxes_ (up : (down : (left : (right : todo))))
                                                                            ((State b (pr,pc)) : visited))
    | otherwise                      = reachableBoxes_ (up : down : left : right : todo)
                                                       ((State b (pr,pc)) : visited)
  where up    = ((State b (pr,pc)), (pr-1,pc))
        down  = ((State b (pr,pc)), (pr+1,pc))
        left  = ((State b (pr,pc)), (pr,pc-1))
        right = ((State b (pr,pc)), (pr,pc+1))

reachableBoxes_ :: [Move] -> [State] -> [State]
reachableBoxes_ [] _ = []
reachableBoxes_ todo visited = 
    case (tryMove (todo!!0)) of
        Just s  -> if (elem s visited)
                       then reachableBoxes_ (tail todo) visited
                       else reachableBoxes s (tail todo) visited
        Nothing -> reachableBoxes_ (tail todo) visited

-- given move,
-- returns updated state if player moved to unoccupied or goal space
tryMove :: Move -> Maybe State
tryMove ((State b (pr,pc)), (r,c))
    | dest == ' ' = Just (State (moveToUnoccupied b (pr,pc) (r,c)) (r,c))
    | dest == '.' = Just (State (moveToGoal b (pr,pc) (r,c)) (r,c))
    | otherwise   = Nothing
  where dest = getCharacter b (r,c)


-- given state,
-- return True if player is adjacent (1 space horizontally or vertically) to a box ($ or *)
       -- False otherwise
isByBox :: State -> Bool
isByBox (State b (pr,pc))
    | up == '$' = True
    | down == '$' = True
    | left  == '$' = True
    | right == '$' = True
    | up == '*' = True
    | down == '*' = True
    | left  == '*' = True
    | right == '*' = True
    | otherwise    = False
  where up    = getCharacter b (pr-1,pc)
        down  = getCharacter b (pr+1,pc)
        left  = getCharacter b (pr,pc-1)
        right = getCharacter b (pr,pc+1)

-- given push,
-- returns updated state if box pushed to unoccupied or goal space
tryPush :: Push -> Maybe State
tryPush ((State b (pr,pc)), (br,bc), (r,c))
    | (dest1 == '$') && (dest2 == ' ') = Just (State (pushToUnoccupied b (pr,pc) (br,bc) (r,c)) (br,bc))
    | (dest1 == '$') && (dest2 == '.') = Just (State (pushToGoal b (pr,pc) (br,bc) (r,c)) (br,bc))
    | (dest1 == '*') && (dest2 == ' ') = Just (State (pushGoalToUnoccupied b (pr,pc) (br,bc) (r,c)) (br,bc))
    | (dest1 == '*') && (dest2 == '.') = Just (State (pushGoalToGoal b (pr,pc) (br,bc) (r,c)) (br,bc))
    | otherwise                        = Nothing
  where dest1 = getCharacter b (br,bc)
        dest2 = getCharacter b (r,c)

-- given a list of states,
-- return a solution (states paired in order as hints)
toSolution :: [State] -> Solution
toSolution [] = []
toSolution (h1:h2:t) = (Hint (h1,h2)) : (toSolution t)

-- given list of pushes,
-- returns list of (push, path)
toPushesTodo :: [Push] -> [State] -> [(Push, [State])]
toPushesTodo lp ls = map (\p -> (p, ls)) lp

-- given a list of states,
-- returns a list of adjacent pushes
toPushes :: [State] -> [Push]
toPushes ls = foldr (\s p -> (toPush s) ++ p) [] ls

-- given a state,
-- returns a list of adjacent pushes (max 4 cardinal directions)
toPush :: State -> [Push]
toPush (State b (pr,pc)) = filter isBoxAt [up, down, left, right]
  where up    = ((State b (pr,pc)), (pr-1,pc), (pr-2,pc))
        down  = ((State b (pr,pc)), (pr+1,pc), (pr+2,pc))
        left  = ((State b (pr,pc)), (pr,pc-1), (pr,pc-2))
        right = ((State b (pr,pc)), (pr,pc+1), (pr,pc+2))

-- given push,
-- returns True if there is a box to push, False otherwise
isBoxAt :: Push -> Bool
isBoxAt ((State b (pr,pc)), (br,bc), (r,c))
    | x == '$'  = True
    | x == '*'  = True
    | otherwise = False
  where x = getCharacter b (br,bc)


{- Deadlock functions -}


-- [TODO]: needs to be called on levels at load (outside hints? don't want to recalc every time 'H' is used),
--         then when boxes are moved (towards-against wall?), check if the position is a dead space

-- given a board,
-- returns a list of deadlock coordinates (the board becomes unsolvable if any box is moved to the coords)
findDeadsquares :: Board -> [Coordinates]
findDeadsquares b = map coords (filter (isDeadSquare b) (findPotentialDeadSquares b))


-- given a board,
-- returns a list of potential dead squares (all ' ' and '@')
findPotentialDeadSquares :: Board -> [DeadSquare]
findPotentialDeadSquares b = [(setDeadSquare b (r,c)) | r <- [0..(length b - 1)],
                                                         c <- [0..(length (head b) - 1)],
                                                         (getCharacter b (r,c) == ' ') || (getCharacter b (r,c) == '@')]

-- given a board and coordinates,
-- returns a potential DeadSquare
setDeadSquare :: Board -> Coordinates -> DeadSquare
setDeadSquare b (r,c) = DeadSquare (r,c) up down left right
  where up    = (getCharacter b (r-1,c)) == '#'
        down  = (getCharacter b (r+1,c)) == '#'
        left  = (getCharacter b (r,c-1)) == '#'
        right = (getCharacter b (r,c+1)) == '#'

-- given a board and a potential dead square,
-- returns True if square is dead, False otherwise
isDeadSquare :: Board -> DeadSquare -> Bool
isDeadSquare b (DeadSquare (r,c) up down left right) = True -- todo


{-- Solver functions --}

-- given state,
-- returns a hint (what box to push next and how)
giveHint :: State -> IO()
giveHint s = print (head (solveLevel s [] [] []))


-- [TODO]: this is taking absurdly long on some inputs due to not checking for deadlock w/ DFS. but it does work otherwise
-- See http://www.sokobano.de/wiki/index.php?title=Deadlocks to check for deadlock? Or make other time improvements. 
-- level3 just doesn't appear to work
-- level5 takes a long time (about 1 minute)

-- given state, path so far, pushes to try, list of visited states,
-- returns a solution
solveLevel :: State -> [State] -> [(Push, [State])] -> [State] -> Solution
solveLevel s path todo visited
    | isGameWon s    = reverse (toSolution path)
    | otherwise      = solveLevel_ (((toPushesTodo (toPushes (reachableBoxes s [] [])) path)) ++ todo) visited

solveLevel_ :: [(Push, [State])] -> [State] -> Solution
solveLevel_ [] _ = []
solveLevel_ ((((State b (pr,pc)), (br,bc), (r,c)), path) : t) visited = 
    case (tryPush ((State b (pr,pc), (br,bc), (r,c)))) of
        Just s  -> if (elem s visited)
                       then solveLevel_ t visited
                       else solveLevel s ((State b (pr,pc)) : s : path) t ((State b (pr,pc)) : s : visited)
        Nothing -> solveLevel_ t visited




{-- Function tests 

> isByBox (State board1 player1)
False

> isByBox (State board2 player2)
True

> tryMove ((State board1 player1), (1,2))
Just...
#########
# @ $  .#
#########

> tryMove ((State board1 player1), (1,0))
Nothing

> reachableBoxes (State board1 player1) [] []
#########
#  @$  .#
#########

> reachableBoxes (State board3 player3) [] []
########
#      #
#  $  .#
# @$  .#
#  $  .#
#      #
########
########
#      #
# @$  .#
#  $  .#
#  $  .#
#      #
########
...etc. (all the way around the boxes)

> toPushes (reachableBoxes (State board3 player3) [] [])

> solveLevel (State board1 player1) [] [] []

> solveLevel (State board5 player5) [] [] [] 
-- This works, but takes a LONG time. (see TODO above)

> giveHint (State board6 player6)

--}