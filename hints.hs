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
findDeadSquares :: Board -> [Coordinates]
findDeadSquares b = map coords (filter (isDeadSquare b) (findPotentialDeadSquares b))


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
isDeadSquare _ (DeadSquare _ False False False False) = False -- space has no adjacent walls
isDeadSquare _ (DeadSquare _ True _    True _   )     = True -- space in corner -- [TODO] this isn't dead if you can slide to a goal.
isDeadSquare _ (DeadSquare _ True _    _    True)     = True -- "
isDeadSquare _ (DeadSquare _ _    True True _   )     = True -- "
isDeadSquare _ (DeadSquare _ _    True _    True)     = True -- "
isDeadSquare b (DeadSquare (r,c) True _ _ _) = (nextWallLeft b (r,c) > (nextLifeLeft b (r,c))) &&
                                               (nextWallRight b (r,c) < (nextLifeRight b (r,c)))
isDeadSquare b (DeadSquare (r,c) _ True _ _) = (nextWallLeft b (r,c) > (nextLifeLeft b (r,c))) && -- wall up/down cases equal
                                               (nextWallRight b (r,c) < (nextLifeRight b (r,c)))
isDeadSquare b (DeadSquare (r,c) _ _ True _) = (nextWallUp b (r,c) > (nextLifeUp b (r,c))) &&
                                               (nextWallDown b (r,c) < (nextLifeDown b (r,c)))
isDeadSquare b (DeadSquare (r,c) _ _ _ True) = (nextWallUp b (r,c) > (nextLifeUp b (r,c))) && -- wall left/right cases equal
                                               (nextWallDown b (r,c) < (nextLifeDown b (r,c)))

-- given board and coordinates,
-- returns row or column of next wall in corresponding direction

nextWallUp :: Board -> Coordinates -> Int
nextWallUp b (r,c)
    | char == '#' = r
    | char == '.' = -1
    | otherwise   = nextWallUp b (r-1,c)
  where char = getCharacter b (r,c)

nextWallDown :: Board -> Coordinates -> Int
nextWallDown b (r,c)
    | char == '#' = r
    | char == '.' = 100000000000 -- [TODO] placeholder. something big (for the comparisons in isDeadSquare)???
    | otherwise   = nextWallDown b (r+1,c)
  where char = getCharacter b (r,c)

nextWallLeft :: Board -> Coordinates -> Int
nextWallLeft b (r,c)
    | char == '#' = r
    | char == '.' = -1
    | otherwise   = nextWallLeft b (r,c-1)
  where char = getCharacter b (r,c)

nextWallRight :: Board -> Coordinates -> Int
nextWallRight b (r,c)
    | char == '#' = r
    | char == '.' = 100000000000 -- [TODO] as above^
    | otherwise   = nextWallRight b (r,c+1)
  where char = getCharacter b (r,c)

{-
nextWallDown :: Board -> Coordinates -> Int
nextWallDown b (r,c) = if ((getCharacter b (r,c)) == '#')
                          then r
                          else nextWallDown b (r+1,c)

nextWallLeft :: Board -> Coordinates -> Int
nextWallLeft b (r,c) = if ((getCharacter b (r,c)) == '#')
                          then c
                          else nextWallLeft b (r,c-1)

nextWallRight :: Board -> Coordinates -> Int
nextWallRight b (r,c) = if ((getCharacter b (r,c)) == '#')
                          then c
                          else nextWallRight b (r,c+1)
-}

-- given board and cooredinates,
-- returns row or column of next 'life' space in the corresponding direction
-- ('life' means there is no deadlock -- the wall ends (' ' or '.') on both sides (so player can push box away))
nextLifeUp :: Board -> Coordinates -> Int
nextLifeUp b (r,c)
    | player == '#'                      = r-1 -- if one exists, it is above the wall (unreachable)
    | ((left == ' ') || (left == '.')) &&
      ((right == ' ') || (right == '.')) = r
    | otherwise                          = nextLifeUp b (r-1,c)
  where player = getCharacter b (r,c)
        left   = getCharacter b (r,c-1)
        right  = getCharacter b (r,c+1)

nextLifeDown :: Board -> Coordinates -> Int
nextLifeDown b (r,c)
    | player == '#'                      = r+1 -- if one exists, it is below the wall (unreachable)
    | ((left == ' ') || (left == '.')) &&
      ((right == ' ') || (right == '.')) = r
    | otherwise                          = nextLifeDown b (r+1,c)
  where player = getCharacter b (r,c)
        left   = getCharacter b (r,c-1)
        right  = getCharacter b (r,c+1)

nextLifeLeft :: Board -> Coordinates -> Int
nextLifeLeft b (r,c)
    | player == '#'                    = c-1 -- if one exists, it is to the left of the wall (unreachable)
    | ((up == ' ') || (up == '.')) &&
      ((down == ' ') || (down == '.')) = c
    | otherwise                        = nextLifeLeft b (r,c-1)
  where player = getCharacter b (r,c)
        up     = getCharacter b (r-1,c)
        down   = getCharacter b (r+1,c)

nextLifeRight :: Board -> Coordinates -> Int
nextLifeRight b (r,c)
    | player == '#'                    = c+1 -- if one exists, it is to the right of the wall (unreachable)
    | ((up == ' ') || (up == '.')) &&
      ((down == ' ') || (down == '.')) = c
    | otherwise                        = nextLifeRight b (r,c+1)
  where player = getCharacter b (r,c)
        up     = getCharacter b (r-1,c)
        down   = getCharacter b (r+1,c)

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