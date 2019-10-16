-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

module Hints where
import Data.Maybe
import Sokoban

type Solution = [State]

type Move = (State, Coordinates)              -- state and dest
type Push = (State, Coordinates, Coordinates) -- state, box coords, and dest

data DeadSquare = DeadSquare -- a potential dead square (the level becomes unsolvable if any box is moved to a dead square)
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
reachableBoxes (State b ps (pr,pc) lvl) todo visited
    | isByBox (State b ps (pr,pc) lvl) = (State b ps (pr,pc) lvl) : (reachableBoxes_ (up : (down : (left : (right : todo))))
                                                                                     ((State b ps (pr,pc) lvl) : visited))
    | otherwise                        = reachableBoxes_ (up : down : left : right : todo)
                                                         ((State b ps (pr,pc) lvl) : visited)
  where up    = ((State b ps (pr,pc) lvl), (pr-1,pc))
        down  = ((State b ps (pr,pc) lvl), (pr+1,pc))
        left  = ((State b ps (pr,pc) lvl), (pr,pc-1))
        right = ((State b ps (pr,pc) lvl), (pr,pc+1))

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
tryMove ((State b ps (pr,pc) lvl), (r,c))
    | dest == ' ' = Just (State (moveToUnoccupied b (pr,pc) (r,c)) (State b ps (pr,pc) lvl) (r,c) lvl)
    | dest == '.' = Just (State (moveToGoal b (pr,pc) (r,c)) (State b ps (pr,pc) lvl) (r,c) lvl)
    | otherwise   = Nothing
  where dest = getCharacter b (r,c)


-- given state,
-- return True if player is adjacent (1 space horizontally or vertically) to a box ($ or *)
       -- False otherwise
isByBox :: State -> Bool
isByBox (State b ps (pr,pc) lvl)
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
tryPush ((State b ps (pr,pc) lvl), (br,bc), (r,c))
    | (dest1 == '$') && (dest2 == ' ') = Just (State (pushToUnoccupied b (pr,pc) (br,bc) (r,c)) (State b ps (pr,pc) lvl) (br,bc) lvl)
    | (dest1 == '$') && (dest2 == '.') = Just (State (pushToGoal b (pr,pc) (br,bc) (r,c)) (State b ps (pr,pc) lvl) (br,bc) lvl)
    | (dest1 == '*') && (dest2 == ' ') = Just (State (pushGoalToUnoccupied b (pr,pc) (br,bc) (r,c)) (State b ps (pr,pc) lvl) (br,bc) lvl)
    | (dest1 == '*') && (dest2 == '.') = Just (State (pushGoalToGoal b (pr,pc) (br,bc) (r,c)) (State b ps (pr,pc) lvl) (br,bc) lvl)
    | otherwise                        = Nothing
  where dest1 = getCharacter b (br,bc)
        dest2 = getCharacter b (r,c)

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
toPush (State b ps (pr,pc) lvl) = filter isBoxAt [up, down, left, right]
  where up    = ((State b ps (pr,pc) lvl), (pr-1,pc), (pr-2,pc))
        down  = ((State b ps (pr,pc) lvl), (pr+1,pc), (pr+2,pc))
        left  = ((State b ps (pr,pc) lvl), (pr,pc-1), (pr,pc-2))
        right = ((State b ps (pr,pc) lvl), (pr,pc+1), (pr,pc+2))

-- given push,
-- returns True if there is a box to push, False otherwise
isBoxAt :: Push -> Bool
isBoxAt ((State b ps (pr,pc) lvl), (br,bc), (r,c))
    | x == '$'  = True
    | x == '*'  = True
    | otherwise = False
  where x = getCharacter b (br,bc)


{- Deadlock functions -}

-- given a board,
-- returns a list of deadlock coordinates (the board becomes unsolvable if any box is moved to the coords)
findDeadSquares :: Board -> [Coordinates]
findDeadSquares b = map coords (filter (isDeadSquare b) (findPotentialDeadSquares b))


-- given a board,
-- returns a list of potential dead squares (all ' ' and '@')
findPotentialDeadSquares :: Board -> [DeadSquare]
findPotentialDeadSquares b = [(createDeadSquare b (r,c)) | r <- [0..(length b - 1)],
                                                           c <- [0..(length (head b) - 1)],
                                                           (getCharacter b (r,c) == ' ') || (getCharacter b (r,c) == '@')]

-- given a board and coordinates,
-- returns a potential DeadSquare
createDeadSquare :: Board -> Coordinates -> DeadSquare
createDeadSquare b (r,c) = DeadSquare (r,c) up down left right
  where up    = (getCharacter b (r-1,c)) == '#'
        down  = (getCharacter b (r+1,c)) == '#'
        left  = (getCharacter b (r,c-1)) == '#'
        right = (getCharacter b (r,c+1)) == '#'

-- given a board and a potential dead square,
-- returns True if square is dead, False otherwise
isDeadSquare :: Board -> DeadSquare -> Bool
isDeadSquare _ (DeadSquare _     False False False False) = False -- space has no adjacent walls
isDeadSquare _ (DeadSquare _     True _    True _   )     = True  -- space in corner (up/left)
isDeadSquare _ (DeadSquare _     True _    _    True)     = True  -- "                up/right
isDeadSquare _ (DeadSquare _     _    True True _   )     = True  -- "                down/left
isDeadSquare _ (DeadSquare _     _    True _    True)     = True  -- "                down/right
isDeadSquare b (DeadSquare (r,c) True _    _    _   )     = (nextWallLeft b (r,c) > (nextLifeLeft b (r,c))) &&
                                                            (nextWallRight b (r,c) < (nextLifeRight b (r,c)))
isDeadSquare b (DeadSquare (r,c) _    True _    _   )     = (nextWallLeft b (r,c) > (nextLifeLeft b (r,c))) && -- wall up/down cases equal
                                                            (nextWallRight b (r,c) < (nextLifeRight b (r,c)))
isDeadSquare b (DeadSquare (r,c) _    _    True _   )     = (nextWallUp b (r,c) > (nextLifeUp b (r,c))) &&
                                                            (nextWallDown b (r,c) < (nextLifeDown b (r,c)))
isDeadSquare b (DeadSquare (r,c) _    _    _    True)     = (nextWallUp b (r,c) > (nextLifeUp b (r,c))) && -- wall left/right cases equal
                                                            (nextWallDown b (r,c) < (nextLifeDown b (r,c)))

-- given board and coordinates,
-- returns row or column of next wall in corresponding direction
nextWallUp :: Board -> Coordinates -> Int
nextWallUp b (r,c)
    | char == '#' = r
    | char == '.' = -1
    | char == '*' = -1
    | otherwise   = nextWallUp b (r-1,c)
  where char = getCharacter b (r,c)

nextWallDown :: Board -> Coordinates -> Int
nextWallDown b (r,c)
    | char == '#' = r
    | char == '.' = 100000000000 -- something greater than the max level height
    | char == '*' = 100000000000
    | otherwise   = nextWallDown b (r+1,c)
  where char = getCharacter b (r,c)

nextWallLeft :: Board -> Coordinates -> Int
nextWallLeft b (r,c)
    | char == '#' = c
    | char == '.' = -1
    | char == '*' = -1
    | otherwise   = nextWallLeft b (r,c-1)
  where char = getCharacter b (r,c)

nextWallRight :: Board -> Coordinates -> Int
nextWallRight b (r,c)
    | char == '#' = c
    | char == '.' = 100000000000 -- something greater than the max level width
    | char == '*' = 100000000000
    | otherwise   = nextWallRight b (r,c+1)
  where char = getCharacter b (r,c)

-- given board and coordinates,
-- returns row or column of next 'life' space in the corresponding direction
-- ('life' means there is no deadlock -- the wall ends (' ' or '.') on both sides (so player can push box away))
nextLifeUp :: Board -> Coordinates -> Int
nextLifeUp b (r,c)
    | box == '#'                      = r-1 -- if one exists, it is above the wall (unreachable)
    | ((left == ' ') || (left == '.')) &&
      ((right == ' ') || (right == '.')) = r
    | otherwise                          = nextLifeUp b (r-1,c)
  where box   = getCharacter b (r,c)
        left  = getCharacter b (r,c-1)
        right = getCharacter b (r,c+1)

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
giveHint :: State -> State
giveHint s = (solveLevel s (findDeadSquares (board s)) [] [] [])!!1


-- [TODO]: time still could use improving
-- also, if 'H' is used sequentially, this still recalculates--unnecessary? (also means that repeated use of 'H' does not necessarily lead to a solution)

-- given state, list of dead space coordinates, path so far, pushes to try, list of visited states,
-- returns a solution
solveLevel :: State -> [Coordinates] -> [State] -> [(Push, [State])] -> [State] -> Solution
solveLevel s deadsquares path todo visited
    | isGameWon s    = reverse path
    | otherwise      = solveLevel_ deadsquares (((toPushesTodo (toPushes (reachableBoxes s [] [])) path)) ++ todo) visited

solveLevel_ :: [Coordinates] -> [(Push, [State])] -> [State] -> Solution
solveLevel_ _ [] _ = []
solveLevel_ deadsquares ((((State b ps (pr,pc) lvl), (br,bc), (r,c)), path) : t) visited = 
    case (tryPush ((State b ps (pr,pc) lvl), (br,bc), (r,c))) of
        Just s  -> if ((elem s visited) || (elem (r,c) deadsquares))
                       then solveLevel_ deadsquares t visited
                       else solveLevel s deadsquares (s : (State b ps (pr,pc) lvl) : path) t ((State b ps (pr,pc) lvl) : s : visited)
        Nothing -> solveLevel_ deadsquares t visited


{-- Function tests 

> isByBox (State board1 Empty player1 "1")
False

> isByBox (State board2 Empty player2 "2")
True

> tryMove ((State board1 Empty player1 "1"), (1,2))
Just...
#########
# @ $  .#
#########

> tryMove ((State board1 Empty player1 "1"), (1,0))
Nothing

> reachableBoxes (State board1 Empty player1 "1") [] []
#########
#  @$  .#
#########

> reachableBoxes (State board3 Empty player3 "3") [] []
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

> toPushes (reachableBoxes (State board3 Empty player3 "3") [] [])

> solveLevel (State board1 Empty player1 "1") (findDeadSquares board1) [] [] []

> solveLevel (State board5 Empty player5 "5") (findDeadSquares board5) [] [] []

> giveHint (State board5 Empty player5 "5") (findDeadSquares board5)

--}