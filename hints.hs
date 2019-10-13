-- CPSC 312 - 2019 - Project 1 - Sokoban
-- Authors: Rebecca Li, Jason Doo, Gurveer Aulakh

-- TODO: reorganise (types in a separate file so that both sokoban.hs and hints.hs can import it? since ultimately sokoban imports hints)
--       ... or move all to sokoban.hs

module Hints where
import Sokoban
import Data.Maybe

data Solution = Solution [Hint] -- a list of hint moves leading to a winning state
  deriving (Eq)                  -- alternatively, a list of states (?)

instance Show Solution where
    show (Solution hints) =
        foldl (\acc hint -> acc ++ "\n" ++ (show hint)) "" hints

data Hint = Hint (State, State) -- the states before and after a suggested box push
  deriving (Eq)

instance Show Hint where
    show (Hint (s1, s2)) = 
        "HINT\n\n" ++ 
        "Before Push:\n" ++ show s1 ++ "\n\n" ++ "After Push:\n" ++ show s2

type Move = (State, Coordinates)

{-- Helper functions --}

-- given state,
-- return True if player is adjacent (1 space horizontally or vertically) to a box ($ or *)
       -- False otherwise
isByBox :: State -> Bool
isByBox (State b (pr,pc))
    | above == '$' = True
    | below == '$' = True
    | left  == '$' = True
    | right == '$' = True
    | above == '*' = True
    | below == '*' = True
    | left  == '*' = True
    | right == '*' = True
    | otherwise    = False
    where above = getCharacter b (pr-1,pc)
          below = getCharacter b (pr+1,pc)
          left  = getCharacter b (pr, pc-1)
          right = getCharacter b (pr, pc+1)




-- given state and list of visited states,
-- returns list of reachable states with player adjacent to a box (moving only to unoccupied ' ' or goal '.' spaces)
reachableBoxes :: State -> [Move] -> [State] -> [State]
reachableBoxes (State b (pr,pc)) todo visited
    | isByBox (State b (pr,pc))      = (State b (pr,pc)) : (reachableBoxes_ (above:(below:(left:(right:todo)))) ((State b (pr,pc)):visited))
    | otherwise                      = reachableBoxes_ (above:(below:(left:(right:todo)))) ((State b (pr,pc)):visited)
    where above = ((State b (pr,pc)), (pr-1,pc))
          below = ((State b (pr,pc)), (pr+1,pc))
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


-- given state and destination coordinates,
-- returns updated state if player moved to unoccupied or goal space
tryMove :: Move -> Maybe State
tryMove ((State b (pr,pc)), (r,c))
    | dest == ' ' = Just (State (moveToUnoccupied b (pr,pc) (r,c)) (r,c))
    | dest == '.' = Just (State (moveToGoal b (pr,pc) (r,c)) (r,c))
    | otherwise   = Nothing
    where dest = getCharacter b (r,c)






{-- Solver functions (TODO) --}

-- given state,
-- returns a solution
--solveLevel :: State -> Solution

-- given solution,
-- returns a hint (what box to push next and how)
--giveHint :: Solution -> Hint


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

> reachableBoxes (State board3 player3)
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

--}