-- utility code specifically for working with data structures from Structs
module Util ( aPrint, oPrint, actor, help, buyCard, isKingdom, isOver, calcVP,
              getWinner, drawCard ) where

import System.IO
import Text.Printf
import Data.Maybe
import qualified Data.Map as M

import Structs
import Misc

-- writes a message to the player whose turn it is
aPrint :: Game -> String -> IO ()
aPrint (Game _ _ usrs Turn{user=usr} _) msg = hPutStr (fst $ io $ usrs!!usr) msg

-- writes a message to everyone except the player whose turn it is
oPrint :: Game -> String -> IO ()
oPrint (Game _ _ usrs Turn{user=usr} _) msg =
    mapM_ (`hPutStr` msg) others
    where others = [fst $ io $ usrs!!i | i <- [0..length usrs - 1], i /= usr]

-- gets the player whose turn it is
actor :: Game -> User
actor Game{users=usrs, turn=Turn{user=usr}} = usrs !! usr

-- prints a list of commands
-- TODO: actually implement
-- TODO: also implement the things it's documenting :P
--   TODO: info command
help :: IO ()
help = return ()

-- returns a new game state where the active user has purchased the given card
--  c   - the name of the card being purchased
--  amt - the amount of the card being purchased in the pile
buyCard :: Game -> String -> Int -> Game
buyCard game@Game{turn=trn@(Turn usr bys gld act), amounts = amts} c amt =
    game{amounts = M.insert c (amt-1) amts,
         turn = trn{buys = bys-1, gold = gld-cardCost}}
    where cardCost = cost $ cards game M.! c

-- Checks whether or not a given card is a kingdom card
-- TODO: maybe use a better way of checking this? TODO more than base
isKingdom :: String -> Bool
isKingdom str = not $ elem str ["copper", "silver", "gold", "curse", "estate",
                                "duchy", "province"]

-- true iff the given game is over (endgame conditions have been met)
isOver :: Game -> Bool
isOver game@(Game crds amts usrs (Turn usr buys gold acts) _) =
    amts M.! "province" == 0 ||
    (M.size $ M.filterWithKey (\k v -> isKingdom k && v /= 0) amts) >= 3

-- calculates the victory points for a single player in the game
--  game - the game state
--  user - the index of the player in (users game)
calcVP :: Game -> Int -> Int
calcVP game@Game{cards = crds, users = usrs} usr =
    sum $ map (\c -> vps (crds M.! c) game') $ hand $ usrs !! usr
    where game' = game{turn = Turn usr 0 0 0}

-- calculates victory points and produces the winner or the game, and their VPs
-- TODO: ties
getWinner :: Game -> (Int, Int)
getWinner game =
    foldr (\x y@(_,vp) -> if calcVP game x > vp then (x, calcVP game x) else y)
          (0, -99999) [0..length (users game) - 1]

-- TODO: maybe handle the case where the deck and discard pile are both empty?
-- draws n cards for the given user
--  n   - the number of cards to draw
--  usr - the index of the user drawing the card in Game{users}
drawCard :: Game -> Int -> Int -> Game
drawCard game 0 _ = game
drawCard game@(Game crds amts usrs _ rnd) n usr = 
    game{users = take usr usrs ++ [newUsr] ++ drop (usr+1) usrs, rand = rnd'}
    where u@(User _ hnd dck dsc _) = usrs !! usr
          ((c:cs, rnd'), dsc') = if null dck then (shuf rnd dsc, [])
                                             else ((dck, rnd), dsc)
          newUsr = u{hand = c:hnd, deck = cs, disc = dsc'}
