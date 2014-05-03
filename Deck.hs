-- code for constructing decks
module Deck ( simpleAct, jsa ) where

import Structs
import Util

-- constructs an action function which only adds buys, gold, draws, and actions
simpleAct :: Int -> Int -> Int -> Int -> Game -> IO Game
simpleAct b g d a game@Game{turn=Turn usr buys gold acts} = return $
    drawCard game{turn = Turn usr (buys+b) (gold+g) (acts+a-1)} d usr

-- wrap the above in a Just
jsa :: Int -> Int -> Int -> Int -> Maybe (Game -> IO Game)
jsa b g d a = Just (\game -> simpleAct b g d a game)
