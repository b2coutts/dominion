-- contains the main high-level code for running the game
module Engine ( simGame ) where

import System.IO
import Text.Printf
import Data.Maybe
import qualified Data.Map as M

import Structs
import Util

-- simulates the action phase of a user
actPhase :: Game -> IO Game
actPhase game@(Game crds amts usrs (Turn usr bys gld acts) _)
    | acts == 0 = return game
    | otherwise = do
        c <- prompt game usr msg echeck
        if c == "/end" then return game else case func $ crds <> c of
            Nothing -> printf "'%s' is not an action card!" c >> actPhase game
            Just fn -> do
                aPrint game $ printf "Using '%s'.\n" c
                oPrint game $ printf "%s uses '%s'.\n" nm c
                game' <- fn $ discard game c
                actPhase game'
    where (User nm hnd dck dsc oi) = usrs !! usr
          msg = printf "You have %d buys, %d bonus gold, and %d actions. Your\
                      \ cards are %s. Which card will you play?"
                        bys gld acts (show hnd)
          echeck "/end" = Nothing
          echeck crd
            | crd `elem` hnd = case func $ crds <> crd of
                Nothing -> Just $ printf "'%s' is not an action card!" crd
                Just _  -> Nothing
            | otherwise      = Just $ printf "'%s' is not in your hand!" crd


-- simulates the buy phase of a user
buyPhase :: Game -> IO Game
buyPhase game@Game{turn = Turn{buys = 0}} = return game
buyPhase game@(Game crds amts usrs (Turn usr bys gld acts) _) = do
    c <- prompt game usr msg echeck
    if c == "/end" then return game else do
        aPrint game $ printf "Purchased card '%s'.\n" c
        oPrint game $ printf "%s purchased '%s'.\n" name c
        buyPhase $ buyCard game c
    where (User name hnd dck dsc oi) = usrs !! usr
          msg = printf "You have %d buys and %d gold. Which card will you\
                      \ buy?" bys gld
          echeck "/end" = Nothing
          echeck crd = case M.lookup crd amts of
            Nothing -> Just $ printf "The card '%s' isn't in this game." crd
            Just 0  -> Just $ printf "There are no more of '%s' left." crd
            Just _  | (cost $ crds <> crd) <= gld -> Nothing
                    | otherwise -> Just $
                        printf "%s costs %d, but you only have %d."
                               crd (cost $ crds <> crd) gld

-- simulates the entire game, eventually producing the ending state
simGame :: Game -> IO Game
simGame game@(Game crds amts usrs trn _)
    | isOver game = let (usri, vp) = getWinner game
                        ig = game{turn=trn{user=usri}} in do
        aPrint ig $ printf "Congrats! You won with %d victory points!\n" vp
        oPrint ig $ printf "%s won the game with %d victory points!\n"
                           (name $ usrs !! usri) vp
        aPrint ig $ finalScore ig
        oPrint ig $ finalScore ig
        return game
    | otherwise = do
        aPrint game $ "Your turn is starting.\n"
        oPrint game $ printf "%s's turn is starting.\n" (name $ actor game)
        game'@Game{turn=trn'@(Turn usr _ gld _)} <- actPhase game
        let endGold = sum $ map (valu . (crds <>)) $ hand $ actor game'
        oPrint game $ printf "%s's action phase is over. Their hand is %s\n"
                             (name $ actor game) (show $ hand $ actor game)
        game'' <- buyPhase game'{turn=trn'{gold = gld + endGold}}
        let draw = flushHand game'' usr
        simGame draw{turn = Turn (mod (usr+1) $ length usrs) 1 0 1}
