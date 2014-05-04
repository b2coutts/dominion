-- contains the main high-level code for running the game
module Engine ( simGame ) where

import System.IO
import Text.Printf
import Data.Maybe
import qualified Data.Map as M

import Structs
import Util

-- simulates the action phase of a user
-- TODO: pretty-print user's hand
actPhase :: Game -> IO Game
actPhase game@(Game crds amts usrs (Turn usr buys gold acts) _)
    | acts == 0 = return game
    -- | null $ catMaybes $ map (func . (crds <>)) hand =  return game
    | otherwise = do
        aPrint game $ printf "You have %d buys, %d bonus gold, and %d actions.\
            \ Your cards are %s. Which card will you play? Type help for a\
            \ command list.\n" buys gold acts (show hand)
        cmd <- hGetLine $ snd h
        case words cmd of
            "play":c:_
                | not $ c `elem` hand -> do
                    aPrint game $ printf "You don't have the card '%s'!\n" c
                    actPhase game
                | isNothing fn -> do
                    aPrint game $ printf "'%s' is not an action card!\n" c
                    actPhase game
                | otherwise -> do
                    aPrint game $ printf "Using '%s'.\n" c
                    oPrint game $ printf "%s uses '%s'.\n" name c
                    game' <- fromJust fn (discard game c)
                    actPhase game'
                where fn = func $ crds <> c
            "end":_ -> return game
            "buy":_ -> do
                aPrint game "You can't buy cards during the action phase.\n"
                actPhase game
            "hand":_ -> do
                aPrint game $ printf "Your hand is: %s.\n" (show hand)
                actPhase game
            "list":_ -> do
                aPrint game $ shopList game
                actPhase game
            "help":_ -> help game >> actPhase game
            cmd:_ -> do
                aPrint game $ "Unknown command: " ++ cmd ++ "\n"
                help game
                actPhase game
            [] -> actPhase game
    where (User name hand deck disc h) = usrs !! usr

-- simulates the buy phase of a user
buyPhase :: Game -> IO Game
buyPhase game@Game{turn = Turn{buys = 0}} = return game
buyPhase game@(Game crds amts usrs (Turn usr buys gold acts) _) = do
    aPrint game $ printf "You have %d buys and %d gold. Which card will you\
                         \ buy?\n" buys gold
    cmd <- hGetLine $ snd h
    case words cmd of
        "buy":c:_ -> case M.lookup c amts of
            Nothing -> do
                aPrint game $ printf "The card '%s' isn't in this game.\n" c
                buyPhase game
            Just 0 -> do
                aPrint game $ printf "There are no more of '%s' left.\n" c
                buyPhase game
            Just amt -> if (cost $ crds <> c) <= gold
                then do
                    aPrint game $ printf "Purchased card '%s'.\n" c
                    oPrint game $ printf "%s purchased '%s'.\n" name c
                    buyPhase $ buyCard game c amt
                else do
                    aPrint game $ printf "%s costs %d, but you only have %d.\n"
                                         c (cost $ crds <> c) gold
                    buyPhase game
        "end":_ -> return game
        "play":_ -> do
            aPrint game $ printf "You can't play cards during the buy phase.\n"
            buyPhase game
        "hand":_ -> do
            aPrint game $ printf "Your hand is: %s.\n" (show hand)
            buyPhase game
        "list":_ -> do
            aPrint game $ shopList game
            buyPhase game
        "help":_ -> help game >> buyPhase game
        cmd:_ -> do
            aPrint game $ "Unknown command: " ++ cmd ++ "\n"
            help game
            buyPhase game
        [] -> buyPhase game
    where (User name hand deck disc h) = usrs !! usr

-- simulates the entire game, eventually producing the ending state
simGame :: Game -> IO Game
simGame game@(Game crds amts usrs trn _)
    | isOver game = let (usri, vp) = getWinner game
                        ig = game{turn=trn{user=usri}} in do
        aPrint ig $ printf "Congrats! You won with %d victory points!\n" vp
        oPrint ig $ printf "%s won the game with %d victory points!\n"
                           (name $ usrs !! usri) vp
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
