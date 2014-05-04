module Structs ( Card(..), User(..), Turn(..), Game(..) ) where

import Data.Map
import System.Random
import System.IO

-- TODO: human-readable card description
-- data structure representing a single card in the game
--  cost - the cost to buy this card from the store
--  valu - the amount of gold this card provides when left unused in the hand
--  vps  - a function that takes the ending game state and produces the number
--         of victory points this card is worth. The user who owns this card
--         will be the "active" (turn) user in the game state
--  func - a function that takes the game state and produces an IO Game, which
--         (maybe) interacts with the users to apply the effect of the card,
--         producing the new game state
data Card = Card { cost :: Int
                 , valu :: Int
                 , vps  :: Either Int (Game -> Int)
                 , func :: Maybe (Game -> IO Game)
                 }
instance Show Card where
    show (Card c v _ _) = "Card(" ++ show c ++ "," ++ show v ++ ")"

-- name - the name of the user
-- hand - a list of the cards in the user's hand
-- deck - a list of the cards in the user's deck
-- disc - a list of the cards in the user's discard pile
-- io   - handles to talk to the user; (toUser, fromUser)
data User = User { name :: String
                 , hand :: [String]
                 , deck :: [String]
                 , disc :: [String]
                 , io   :: (Handle, Handle)
                 } deriving (Show)

-- user - the index of the user whose turn it is in the users field of Game
-- buys - the number of buys in the current turn
-- gold - the amount of bonus gold in the current turn
-- acts - the number of free actions in the current turn
data Turn = Turn { user :: Int
                 , buys :: Int
                 , gold :: Int
                 , acts :: Int
                 } deriving (Show)

-- cards   - a dictionary mapping names of cards in this game to Cards
-- amounts - a dictionary mapping names of cards in this game to the current
--           size of their shop piles
-- users   - a list of the players in this game, ordered by turn order
-- turn    - the state of the current turn
-- rand    - a random number generator
data Game = Game { cards   :: Map String Card
                 , amounts :: Map String Int
                 , users   :: [User]
                 , turn    :: Turn
                 , rand    :: StdGen
                 } deriving (Show)
