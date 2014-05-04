-- code for the base dominion set
module Base ( baseSet ) where

import Data.Map

import Deck
import Structs
import Util

zero :: Either Int (Game -> Int)
zero = Left 0

-- gardens value function; gets a player's deck size modulo 10
gardensVP :: User -> Int
gardensVP (User _ hnd dck dsc _) = mod (length hnd + length dck + length dsc) 10

-- TODO: complicated kingdom cards
baseSet :: Map String Card
baseSet = fromList
  -- basic cards
  [ ("copper",   Card 0 1 zero Nothing)
  , ("silver",   Card 3 2 zero Nothing)
  , ("gold",     Card 6 3 zero Nothing)
  , ("curse",    Card 0 0 (Left (-1)) Nothing)
  , ("estate",   Card 2 0 (Left 1) Nothing)
  , ("duchy",    Card 5 0 (Left 3) Nothing)
  , ("province", Card 8 0 (Left 6) Nothing)

  -- gardens are weird, yo
  , ("gardens", Card 4 0 (Right $ gardensVP . actor) Nothing)

  -- simple kingdom cards
  , ("moat",       Card 2 0 zero $ jsa 0 0 2 0)
  , ("village",    Card 3 0 zero $ jsa 0 0 1 2)
  , ("woodcutter", Card 3 0 zero $ jsa 1 2 0 0)
  , ("smithy",     Card 4 0 zero $ jsa 0 0 3 0)
  , ("festival",   Card 5 0 zero $ jsa 1 2 0 2)
  , ("laboratory", Card 5 0 zero $ jsa 0 0 2 1)
  , ("market",     Card 5 0 zero $ jsa 1 1 1 1)
  ]
