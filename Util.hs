-- utility code specifically for working with data structures from Structs
module Util ( aPrint, oPrint, actor, help, buyCard, isKingdom, isOver, calcVP,
              getWinner, drawCard, (<>), flushHand, shopList, discard ) where

import GHC.Exts
import System.IO
import Text.Printf
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Structs
import Misc

-- TODO: remove this function
(<>) :: (Ord k, Show k, Show a) => M.Map k a -> k -> a
m <> k = case M.lookup k m of
    Just v  -> v
    Nothing -> error $ "MISS: map is " ++ show m ++ ", key is" ++ show k

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

-- given a game state, "apply" the given function to the active user
modActor :: Game -> (User -> User) -> Game
modActor game@Game{users=usrs, turn=Turn{user=usr}} f =
    game{users = take usr usrs ++ [f $ usrs !! usr] ++ drop (usr+1) usrs}

-- prints a list of commands
-- TODO: actually implement
-- TODO: also implement the things it's documenting :P
--   TODO: info command
help :: Game -> IO ()
help game = aPrint game $
    "TODO. Available commands: buy, end, play, hand, list, help.\n"

-- returns a new game state where the active user has purchased the given card
--  c   - the name of the card being purchased
--  amt - the amount of c available in the shop
buyCard :: Game -> String -> Int -> Game
buyCard game@Game{turn=trn@(Turn usr bys gld act), amounts = amts} c amt =
    modActor game' (\u -> u{disc = c : disc u})
    where cardCost = cost $ cards game <> c
          game' = game{amounts = M.insert c (amt-1) amts,
                       turn = trn{buys = bys-1, gold = gld-cardCost}}

-- Checks whether or not a given card is a kingdom card
-- TODO: maybe use a better way of checking this? TODO more than base
isKingdom :: String -> Bool
isKingdom str = not $ elem str ["copper", "silver", "gold", "curse", "estate",
                                "duchy", "province"]

-- true iff the given game is over (endgame conditions have been met)
isOver :: Game -> Bool
isOver game@(Game crds amts usrs (Turn usr buys gold acts) _) =
    amts <> "province" == 0 ||
    (M.size $ M.filterWithKey (\k v -> isKingdom k && v == 0) amts) >= 3

-- calculates the victory points for a single player in the game
--  game - the game state
--  user - the index of the player in (users game)
calcVP :: Game -> Int -> Int
calcVP game@Game{cards = crds, users = usrs} usr =
    sum $ map (\c -> vps (crds <> c) game') $ allcards
    where game' = game{turn = Turn usr 0 0 0}
          allcards = concatMap ($ (usrs !! usr)) [hand, disc, deck]

-- a is 
-- mid takes (User -> [String]) to [String], 

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
drawCard game@(Game crds amts usrs _ rnd) n usr = drawCard
    game{users = take usr usrs ++ [newUsr] ++ drop (usr+1) usrs, rand = rnd'}
    (n-1) usr
    where u@(User _ hnd dck dsc _) = usrs !! usr
          ((c:cs, rnd'), dsc') = if null dck then (shuf rnd dsc, [])
                                             else ((dck, rnd), dsc)
          newUsr = u{hand = c:hnd, deck = cs, disc = dsc'}

-- transfer the given user's hand into their discard pile, then draw 5 cards
--  usr - the index of the user in Game{users}
flushHand :: Game -> Int -> Game
flushHand game usr = drawCard game' 5 usr
    where game' = modActor game (\u@(User _ h _ d _) -> u{hand=[], disc=h++d})

-- create a human-readable list of all cards being sold
shopList :: Game -> String
shopList (Game crds amts _ _ _) = "Card                Cost    Amount\n" ++
    concatMap (\c -> printf "%-20s%-8d%d\n" c (cost $ crds M.! c) (amts M.! c))
              (sortWith (cost . (crds M.!)) (M.keys amts))

-- discards one of the given card from the active player's hand
discard :: Game -> String -> Game
discard game c = modActor game (\u@(User _ h _ d _) -> u{hand = delete c h,
                                                         disc = c:d})
