-- code for the base dominion set
module Base ( baseSet ) where

import qualified Data.Map as M
import Data.List
import Text.Printf

import Deck
import Structs
import Util

zero :: Either Int (Game -> Int)
zero = Left 0

-- gardens value function; gets a player's deck size modulo 10
gardensVP :: User -> Int
gardensVP (User _ hnd dck dsc _) = mod (length hnd + length dck + length dsc) 10

-- helper function for cellar; prompts the actor to discard 0 or more cards
--  n - the number of cards discarded so far
cellarAcc :: Int -> Game -> IO Game
cellarAcc n game = do
    crd <- prompt game (user $ turn $ game) msg echeck
    if crd == "/end" then drawCards n game (user $ turn $ game) else do
        aPrint game $ printf "Discarded %s.\n" crd
        oPrint game $ printf "%s discarded %s.\n" nm crd
        cellarAcc (n+1) (discard game crd)
    where (User nm hnd _ dsc oi) = actor game
          msg = printf "Your hand is %s. You have discarded %d cards. Type the\
                       \ name of the card you wish to discard, or /end to stop\
                       \ discarding." (haShow hnd) n
          echeck "/end" = Nothing
          echeck crd = if crd `elem` hnd then Nothing
            else Just $ printf "'%s' is not in your hand!" crd

-- helper function for chapel; prompts the actor to trash up to 4 cards
chapelAcc :: Int -> Game -> IO Game
chapelAcc 4 game = return game
chapelAcc n game = do
    crd <- prompt game (user $ turn $ game) msg echeck
    if crd == "/end" then return $ actDec game else do
        aPrint game $ printf "Trashed %s.\n" crd
        oPrint game $ printf "%s trashed %s.\n" nm crd
        chapelAcc (n+1) $ modActor game $ const u{hand = delete crd hnd}
    where u@(User nm hnd _ dsc oi) = actor game
          msg = printf "Your hand is %s. You have discarded %d cards. Type the\
                      \ name of the card you wish to discard, or /end to stop\
                      \ discarding." (haShow hnd) n
          echeck "/end" = Nothing
          echeck crd = if crd `elem` hnd then Nothing
              else Just $ printf "'%s' is not in your hand!" crd

chancellor :: Game -> IO Game
chancellor game = do
    resp <- prompt game (user $ turn $ game) msg yncheck
    let game' = if resp == "n" then game
        else modActor game $ const u{deck = [], disc = dck++dsc}
    simpleAct 0 2 0 0 game'
    where u@(User nm hnd dck dsc oi) = actor game
          msg = printf "Would you like to discard your deck? [y/n]"

workshop :: Game -> IO Game
workshop game@Game{cards=cs, amounts=as, turn=trn@Turn{gold=gld, buys=bs}} = do
    c <- prompt game (user $ turn $ game) msg echeck
    let game' = game{turn=trn{gold=gld + cost (cs <> c), buys=bs+1}}
    return $ actDec $ buyCard game' c
    where msg = "Choose a card costing up to 4 gold to gain."
          echeck crd = case M.lookup crd as of
            Nothing -> Just $ printf "'%s' isn't a card in this game!" crd
            Just 0  -> Just $ printf "There are no more of %s left." crd
            Just _  -> if (cost $ cs <> crd) <= 4 then Nothing else
                Just $ printf "%s is too expensive." crd

-- assumes the given game state has a feast at the top of the discard pile
feast :: Game -> IO Game
feast game@Game{cards=cs, amounts=as, turn=trn@(Turn usr bys gld act)} = do
    c <- prompt game (user $ turn $ game) msg echeck
    let game'  = game{turn=trn{gold=gld + cost (cs <> c), buys=bys+1}}
        game'' = modActor game' (\u@User{disc=d} -> u{disc=tail d})
    return $ actDec $ buyCard game'' c
    where msg = "Choose a card costing up to 5 gold to gain."
          echeck crd = case M.lookup crd as of
            Nothing -> Just $ printf "'%s' isn't a card in this game!" crd
            Just 0  -> Just $ printf "There are no more of %s left." crd
            Just _  -> if (cost $ cs <> crd) <= 5 then Nothing else
                Just $ printf "%s is too expensive." crd
    
moneylender :: Game -> IO Game
moneylender game@Game{turn=trn@Turn{gold=gld}} = do
    resp <- prompt game (user $ turn $ game) msg yncheck
    if resp == "n" then return $ actDec game else
        return $ actDec $ modActor game{turn=trn{gold=gld+3}}
                          (\u@User{hand=h} -> u{hand = delete "copper" h})
    where msg = printf "Would you like to discard a copper? [y/n]"

remodel :: Game -> IO Game
remodel game@Game{turn=trn} = do
    c <- prompt game (user $ turn $ game) msg echeck
    let gain = cost $ cards game <> c
    return $ actDec $ modActor game{turn=trn{gold=gold trn + gain}}
                      (\u@User{hand=hnd} -> u{hand=delete c hnd})
    where msg = printf "Which card would you like to remodel?"
          echeck crd = if crd `elem` (hand $ actor $ game) then Nothing else
            Just $ printf "'%s' is not in your hand!" crd

-- helper function for councilroom; draws a card for a list of user indices
drawOther :: Game -> [Int] -> IO Game
drawOther g []     = return g
drawOther g (i:is) = drawCard g i >>= flip drawOther is

councilroom :: Game -> IO Game
councilroom game@Game{users=us, turn=Turn{user=u}} = do
    game' <- drawOther game $ [0..length us - 1] \\ [u]
    simpleAct 1 0 4 0 game'

library :: Game -> IO Game
library game@Game{cards = cs, turn = Turn{user = usr}}
    | (length hnd >= 7) || (null dck && null dsc) = return $ actDec game
    | otherwise = do 
        game' <- drawCard game usr
        let c = head $ hand $ actor game'
        case (func $ cs <> c) of
            Nothing -> library game'
            Just _  -> do
                resp <- prompt game usr "Would you like to discard it?" yncheck
                if resp == "n" then library game'
                    else library $ modActor game' (\u@(User _ (c:cs) _ dsc' _)
                        -> u{hand = cs, disc = c:dsc'})
    where User nm hnd dck dsc oi = actor game

baseSet :: M.Map String Card
baseSet = M.fromList
  -- basic cards
  [ ("copper",   Card 0 1 zero Nothing "")
  , ("silver",   Card 3 2 zero Nothing "")
  , ("gold",     Card 6 3 zero Nothing "")
  , ("curse",    Card 0 0 (Left (-1)) Nothing "")
  , ("estate",   Card 2 0 (Left 1) Nothing "")
  , ("duchy",    Card 5 0 (Left 3) Nothing "")
  , ("province", Card 8 0 (Left 6) Nothing "")

  -- gardens are weird, yo
  , ("gardens", Card 4 0 (Right $ gardensVP . actor) Nothing $ dWrap
        "Worth 1 VP for every 10 cards in your deck (rounded down)\n")

  -- simple kingdom cards
  , ("moat",       Card 2 0 zero (jsa 0 0 2 0) $ dWrap
        "When another player plays an Attack card, you may reveal this from \
        \your hand. If you do, you are unaffected by that Attack.\n")
  , ("village",    Card 3 0 zero (jsa 0 0 1 2) $
        "+1 Card\n+2 Actions\n")
  , ("woodcutter", Card 3 0 zero (jsa 1 2 0 0) $
        "+1 Buy\n+2 Gold\n")
  , ("smithy",     Card 4 0 zero (jsa 0 0 3 0) $
        "+3 Cards\n")
  , ("festival",   Card 5 0 zero (jsa 1 2 0 2) $
        "+2 Actions\n+1 Buy\n+2 Gold\n")
  , ("laboratory", Card 5 0 zero (jsa 0 0 2 1) $
        "+2 Cards\n+1 Action\n")
  , ("market",     Card 5 0 zero (jsa 1 1 1 1) $
        "+1 Card\n+1 Action\n+1 Buy\n+1 Gold\n")

  -- complicated kingdom cards
  , ("cellar", Card 2 0 zero (Just $ cellarAcc 0) $ "+1 Action\n\n" ++ dWrap
        "Discard any number of cards. +1 Card per card discarded.\n")
  , ("chapel", Card 2 0 zero (Just $ chapelAcc 0) $ dWrap
        "Trash up to 4 cards from your hand.")
  , ("chancellor", Card 3 0 zero (Just chancellor) $ "+2 Gold\n\n" ++ dWrap
        "You may immediately put your deck into your discard pile.\n")
  , ("workshop", Card 3 0 zero (Just workshop) $ dWrap
        "Gain a card costing up to 4 gold\n")
  , ("feast", Card 4 0 zero (Just feast) $ dWrap
        "Trash this card. Gain a card costing up to 5 Gold.\n")
  , ("moneylender", Card 4 0 zero (Just moneylender) $ dWrap
        "Trash a Copper card from your hand. If you do, +3 Gold.\n")
  , ("remodel", Card 4 0 zero (Just remodel) $ dWrap
        "Trash a card from your hand. Gain a card costing up to 2 gold more\
       \ than the trashed card.\n")
  -- TODO throne room ;_;
  , ("councilroom", Card 5 0 zero (Just councilroom) $ "+4 Cards\n+1 Buy\n\n" ++
        dWrap "Each other player draws a card.")
  , ("library", Card 5 0 zero (Just library) $ dWrap
        "Draw until you have 7 cards in hand. You may set aside any Action\
       \ cards drawn this way, as you draw them; discard the set aside cards\
       \ after you finish drawing.\n")

  -- TODO attack cards
  ]
